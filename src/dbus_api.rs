// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::collections::BTreeMap;
use std::borrow::Borrow;
use std::rc::Rc;
use std::cell::RefCell;
use std::path::PathBuf;
use std::sync::Arc;

use dbus::{Connection, NameFlag};
use dbus::tree::{Factory, Tree, Property, MethodFn, MethodErr, EmitsChangedSignal};
use dbus::MessageItem;

use froyo::Froyo;
use blockdev::BlockMember;
use types::FroyoResult;

#[derive(Debug, Clone)]
pub struct DbusContext<'a> {
    name_prop: Arc<Property<MethodFn<'a>>>,
    pub remaining_prop: Arc<Property<MethodFn<'a>>>,
    pub total_prop: Arc<Property<MethodFn<'a>>>,
    pub status_prop: Arc<Property<MethodFn<'a>>>,
    pub running_status_prop: Arc<Property<MethodFn<'a>>>,
    pub block_devices_prop: Arc<Property<MethodFn<'a>>>,
}

impl<'a> DbusContext<'a> {
    pub fn update_one(prop: &Arc<Property<MethodFn<'a>>>, m: MessageItem)
                      -> FroyoResult<()> {
        prop.set_value(m);
        // TODO: result is signals we need to be sending for PropertyChanged???
        Ok(())
    }

    pub fn get_block_devices_msgitem(block_devs: &BTreeMap<String, BlockMember>)
                                     -> MessageItem {
        let mut msg_vec = Vec::new();
        for (_, bd) in block_devs {
            let (bd_path, bd_status) = match *bd {
                BlockMember::Present(ref bd) =>
                // TODO: in-use vs not-in-use
                    (RefCell::borrow(&bd).path.to_string_lossy().into_owned(), 0u32),
                BlockMember::Absent(ref sbd) =>
                    (sbd.path.to_string_lossy().into_owned(), 3u32),
            };

            let entry = MessageItem::Struct(vec![bd_path.into(), bd_status.into()]);
            msg_vec.push(entry);
        }

        MessageItem::new_array(msg_vec)
            .expect("Froyodev with no blockdev members???")
    }
}

pub fn get_tree<'a>(c: &Connection, froyos: &mut Rc<RefCell<Vec<Rc<RefCell<Froyo<'a>>>>>>)
                       -> FroyoResult<Tree<MethodFn<'a>>> {
    c.register_name("org.freedesktop.Froyo1", NameFlag::ReplaceExisting as u32).unwrap();

    let f = Factory::new_fn();

    let froyos_closed_over = froyos.clone();
    let mut froyos = RefCell::borrow_mut(&froyos);

    let create_method = f.method("Create", move |m,_,_| {
        let mut items = m.get_items();
        if items.len() < 3 {
            return Err(MethodErr::no_arg())
        }

        let force: bool = try!(items.pop().ok_or_else(MethodErr::no_arg)
                               .and_then(|i| i.inner().map_err(|_| MethodErr::invalid_arg(&i))));
        let blockdevs = match try!(items.pop().ok_or_else(MethodErr::no_arg)) {
            MessageItem::Array(x, _) => x,
            x => return Err(MethodErr::invalid_arg(&x)),
        };
        let blockdevs = blockdevs.into_iter()
            .map(|x| PathBuf::from(x.inner::<&str>().unwrap()))
            .collect::<Vec<_>>();

        let name = try!(items.pop().ok_or_else(MethodErr::no_arg)
                        .and_then(|i| i.inner::<&str>()
                                  .map_err(|_| MethodErr::invalid_arg(&i))
                                  .map(|i| i.to_owned())));

        let froyo = match Froyo::create(&name, &blockdevs, force) {
            Ok(x) => x,
            Err(_) => return Err(MethodErr::failed(&"Froyo create failed")),
        };
        try!(froyo.save_state()
             .map_err(|_| MethodErr::failed(&"Froyo saving state failed")));

        let s = format!("/org/freedesktop/froyo/{}", froyo.id);
        let mr = m.method_return().append(s);

        RefCell::borrow_mut(&froyos_closed_over).push(Rc::new(RefCell::new(froyo)));

        Ok(vec![mr])
    })
        .in_arg(("name", "s"))
        .in_arg(("blockdevs", "as"))
        .in_arg(("force", "b"))
        .out_arg(("obj_path", "s"));

    let tree = f.tree();
    let tree = tree
        .add(f.object_path("/org/freedesktop/froyo")
             .introspectable()
             .object_manager()
             .add(f.interface("org.freedesktop.FroyoService1")
                  .add_m(create_method)
             ));

    let tree = froyos
        .iter_mut()
        .fold(tree, |tree, froyo| {

            let mut iface = f.interface("org.freedesktop.FroyoDevice1");
            let name_p = iface.add_p_ref(f.property(
                "Name", RefCell::borrow(&*froyo).name.to_owned()));
            let p_closed_over = name_p.clone();
            let froyo_closed_over = froyo.clone();
            let mut iface = iface.add_m(
                f.method("SetName", move |m,_,_| {
                    let mut items = m.get_items();
                    if items.len() < 1 {
                        return Err(MethodErr::no_arg())
                    }

                    let name = try!(items.pop().ok_or_else(MethodErr::no_arg)
                                    .and_then(|i| i.inner::<&str>()
                                              .map_err(|_| MethodErr::invalid_arg(&i))
                                              .map(|i| i.to_owned())));

                    let mut froyo = RefCell::borrow_mut(&*froyo_closed_over);
                    froyo.name = name.clone();
                    try!(froyo.save_state()
                         .map_err(|_| MethodErr::failed(&"Froyo saving state failed")));

                    try!(p_closed_over.set_value(name.into())
                         .map_err(|_| MethodErr::invalid_arg(&"name")));
                    Ok(vec![m.method_return()])
                })
                    .in_arg(("new_name", "s")));

            let rem_p = iface.add_p_ref(f.property("RemainingSectors", 0u64)
                                        .emits_changed(EmitsChangedSignal::False));
            let tot_p = iface.add_p_ref(f.property("TotalSectors", 0u64)
                                        .emits_changed(EmitsChangedSignal::False));
            let status_p = iface.add_p_ref(f.property("Status", 0u32));
            let running_status_p = iface.add_p_ref(f.property("RunningStatus", 0u32));

            let mut froyo = RefCell::borrow_mut(froyo);

            // Need to actually get values b/c I can't figure out how to
            // get a 0-length array of struct
            let bdev_msg = DbusContext::get_block_devices_msgitem(&froyo.block_devs);
            let block_devices_p = iface.add_p_ref(f.property("BlockDevices", bdev_msg));

            // TODO: AddBlockDevice method
            // TODO: RemoveBlockDevice method
            // TODO: Reshape method

            let path = format!("/org/freedesktop/froyo/{}", froyo.id);
            froyo.dbus_context = Some(DbusContext {
                name_prop: name_p,
                remaining_prop: rem_p,
                total_prop: tot_p,
                status_prop: status_p,
                running_status_prop: running_status_p,
                block_devices_prop: block_devices_p,
            });

            tree.add(f.object_path(path)
                     .introspectable()
                     .add(iface))
        });

    for froyo in &*froyos {
        try!(RefCell::borrow(&froyo).update_dbus());
    }

    tree.set_registered(&c, true).unwrap();

    Ok(tree)
}

