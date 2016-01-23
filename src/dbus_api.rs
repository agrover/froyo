// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::borrow::Borrow;
use std::rc::Rc;
use std::cell::RefCell;
use std::path::PathBuf;

use dbus::{Connection, NameFlag};
use dbus::tree::Factory;
use dbus::tree::Tree;
use dbus::tree::{MethodFn, MethodErr};
use dbus::MessageItem;

use froyo::Froyo;
use types::FroyoResult;

pub fn get_tree<'a>(c: &Connection, froyos: &mut Rc<RefCell<Vec<Rc<RefCell<Froyo>>>>>)
                       -> FroyoResult<Tree<MethodFn<'a>>> {
    c.register_name("org.freedesktop.Froyo1", NameFlag::ReplaceExisting as u32).unwrap();

    let f = Factory::new_fn();

    let froyos_closed_over = froyos.clone();
    let froyos = RefCell::borrow(&froyos);

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
            Err(_) => return Err(MethodErr::failed(&format!("dude"))),
        };

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
        .iter()
        .fold(tree, |tree, froyo| {

            let mut iface = f.interface("org.freedesktop.FroyoDevice1");
            let p = iface.add_p_ref(f.property("Name", RefCell::borrow(&*froyo).name.to_owned()));
            let froyo_closed_over = froyo.clone();
            let iface = iface.add_m(
                f.method("SetName", move |m,_,_| {
                    let mut items = m.get_items();
                    if items.len() < 1 {
                        return Err(MethodErr::no_arg())
                    }

                    let name = try!(items.pop().ok_or_else(MethodErr::no_arg)
                                    .and_then(|i| i.inner::<&str>()
                                              .map_err(|_| MethodErr::invalid_arg(&i))
                                              .map(|i| i.to_owned())));

                    RefCell::borrow_mut(&*froyo_closed_over).name = name.clone();
                    try!(p.set_value(name.into())
                         .map_err(|_| MethodErr::invalid_arg(&"name")));
                    Ok(vec![m.method_return()])
                })
                    .in_arg(("new_name", "s")));

            let path = format!("/org/freedesktop/froyo/{}", RefCell::borrow(&*froyo).id);
            tree.add(f.object_path(path)
                     .introspectable()
                     .add(iface))
        });

    println!("tree {:#?}", tree);
    tree.set_registered(&c, true).unwrap();

    Ok(tree)
}

