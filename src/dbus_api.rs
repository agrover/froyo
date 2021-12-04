// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::cell::RefCell;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::sync::Arc;

use dbus;
use dbus::tree::{EmitsChangedSignal, Factory, Interface, MethodErr, MethodFn, Property, Tree};
use dbus::MessageItem;
use dbus::{Connection, NameFlag};

use blockdev::{BlockDevs, BlockMember};
use froyo::Froyo;
use types::{FroyoError, FroyoResult};

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
    pub fn update_one(prop: &Arc<Property<MethodFn<'a>>>, m: MessageItem) -> FroyoResult<()> {
        match prop.set_value(m) {
            Ok(_) => Ok(()), // TODO: return signals
            Err(()) => Err(FroyoError::Dbus(dbus::Error::new_custom(
                "UpdateError",
                "Could not update property with value",
            ))),
        }
    }

    pub fn get_block_devices_msgitem(block_devs: &BlockDevs) -> MessageItem {
        let mut msg_vec = Vec::new();
        for bd in block_devs.0.values() {
            let (bd_path, bd_status) = match *bd {
                BlockMember::Present(ref bd) => {
                    let bd = bd.borrow();
                    let status = match bd.linear_devs.len() {
                        0 => 1u32, // not in use
                        _ => 0u32, // in use
                    };
                    (bd.path.to_string_lossy().into_owned(), status)
                }
                BlockMember::Absent(ref sbd) => (sbd.path.to_string_lossy().into_owned(), 3u32),
            };

            let entry = MessageItem::Struct(vec![bd_path.into(), bd_status.into()]);
            msg_vec.push(entry);
        }

        MessageItem::new_array(msg_vec).expect("Froyodev with no blockdev members???")
    }
}

fn froyo_interface<'a>(froyo: &Rc<RefCell<Froyo<'a>>>) -> Interface<MethodFn<'a>> {
    let f = Factory::new_fn();
    let mut iface = f.interface("org.freedesktop.FroyoDevice1");
    let name_p = iface.add_p_ref(f.property("Name", froyo.borrow().name.to_owned()));
    let p_closed_over = name_p.clone();
    let froyo_closed_over = froyo.clone();
    let mut iface = iface.add_m(
        f.method("SetName", move |m, _, _| {
            let mut items = m.get_items();
            if items.is_empty() {
                return Err(MethodErr::no_arg());
            }

            let name = items.pop().ok_or_else(MethodErr::no_arg).and_then(|i| {
                i.inner::<&str>()
                    .map_err(|_| MethodErr::invalid_arg(&i))
                    .map(|i| i.to_owned())
            })?;

            let mut froyo = froyo_closed_over.borrow_mut();
            froyo.name = name.clone();
            froyo.save_state().map_err(|err| {
                let msg = format!("Saving state failed: {}", err);
                MethodErr::failed(&msg)
            })?;

            p_closed_over
                .set_value(name.into())
                .map_err(|_| MethodErr::invalid_arg(&"name"))?;
            Ok(vec![m.method_return()])
        })
        .in_arg(("new_name", "s")),
    );

    let rem_p = iface.add_p_ref(
        f.property("RemainingSectors", 0u64)
            .emits_changed(EmitsChangedSignal::False),
    );
    let tot_p = iface.add_p_ref(
        f.property("TotalSectors", 0u64)
            .emits_changed(EmitsChangedSignal::False),
    );
    let status_p = iface.add_p_ref(f.property("Status", 0u32));
    let running_status_p = iface.add_p_ref(f.property("RunningStatus", 0u32));

    let froyo_closed_over = froyo.clone();
    let iface = iface.add_m(
        f.method("AddBlockDevice", move |m, _, _| {
            let mut items = m.get_items();
            if items.len() < 2 {
                return Err(MethodErr::no_arg());
            }

            let force: bool = items
                .pop()
                .ok_or_else(MethodErr::no_arg)
                .and_then(|i| i.inner().map_err(|_| MethodErr::invalid_arg(&i)))?;

            let new_dev = items.pop().ok_or_else(MethodErr::no_arg).and_then(|i| {
                i.inner::<&str>()
                    .map_err(|_| MethodErr::invalid_arg(&i))
                    .map(|i| i.to_owned())
            })?;

            let mut froyo = froyo_closed_over.borrow_mut();
            froyo
                .add_block_device(Path::new(&new_dev), force)
                .map_err(|err| {
                    let msg = format!("Adding block device failed: {}", err);
                    MethodErr::failed(&msg)
                })?;
            froyo.save_state().map_err(|err| {
                let msg = format!("Saving state failed: {}", err);
                MethodErr::failed(&msg)
            })?;
            Ok(vec![m.method_return()])
        })
        .in_arg(("device_path", "s"))
        .in_arg(("force", "b")),
    );

    let froyo_closed_over = froyo.clone();
    let iface = iface.add_m(
        f.method("RemoveBlockDevice", move |m, _, _| {
            let mut items = m.get_items();
            if items.is_empty() {
                return Err(MethodErr::no_arg());
            }

            let wipe: bool = items
                .pop()
                .ok_or_else(MethodErr::no_arg)
                .and_then(|i| i.inner().map_err(|_| MethodErr::invalid_arg(&i)))?;

            let removing_dev = items.pop().ok_or_else(MethodErr::no_arg).and_then(|i| {
                i.inner::<&str>()
                    .map_err(|_| MethodErr::invalid_arg(&i))
                    .map(|i| i.to_owned())
            })?;

            let mut froyo = froyo_closed_over.borrow_mut();
            froyo
                .remove_block_device(Path::new(&removing_dev), wipe)
                .map_err(|err| {
                    let msg = format!("Removing block device failed: {}", err);
                    MethodErr::failed(&msg)
                })?;
            froyo.save_state().map_err(|err| {
                let msg = format!("Saving state failed: {}", err);
                MethodErr::failed(&msg)
            })?;
            Ok(vec![m.method_return()])
        })
        .in_arg(("device_path", "s"))
        .in_arg(("wipe", "b")),
    );

    let froyo_closed_over = froyo.clone();
    let mut iface = iface.add_m(f.method("Reshape", move |m, _, _| {
        let mut froyo = froyo_closed_over.borrow_mut();
        froyo.reshape().map_err(|err| {
            let msg = format!("Reshape failed: {}", err);
            MethodErr::failed(&msg)
        })?;
        Ok(vec![m.method_return()])
    }));

    let mut froyo = froyo.borrow_mut();

    // Need to actually get values b/c I can't figure out how to
    // get a 0-length array of struct
    let bdev_msg = DbusContext::get_block_devices_msgitem(&froyo.block_devs);
    let block_devices_p = iface.add_p_ref(f.property("BlockDevices", bdev_msg));

    froyo.dbus_context = Some(DbusContext {
        name_prop: name_p,
        remaining_prop: rem_p,
        total_prop: tot_p,
        status_prop: status_p,
        running_status_prop: running_status_p,
        block_devices_prop: block_devices_p,
    });

    iface
}

pub fn get_base_tree<'a>(
    c: &'a Connection,
    froyos: &mut Rc<RefCell<Vec<Rc<RefCell<Froyo<'a>>>>>>,
    child_tree: &Rc<RefCell<Tree<MethodFn<'a>>>>,
) -> FroyoResult<Tree<MethodFn<'a>>> {
    c.register_name("org.freedesktop.Froyo1", NameFlag::ReplaceExisting as u32)
        .unwrap();

    let f = Factory::new_fn();

    let base_tree = f.tree();

    let tree_closed_over = child_tree.clone();
    let froyos_closed_over = froyos.clone();
    let create_method = f
        .method("Create", move |m, _, _| {
            let f = Factory::new_fn();
            let mut items = m.get_items();
            if items.len() < 3 {
                return Err(MethodErr::no_arg());
            }

            let force: bool = items
                .pop()
                .ok_or_else(MethodErr::no_arg)
                .and_then(|i| i.inner().map_err(|_| MethodErr::invalid_arg(&i)))?;
            let blockdevs = match items.pop().ok_or_else(MethodErr::no_arg)? {
                MessageItem::Array(x, _) => x,
                x => return Err(MethodErr::invalid_arg(&x)),
            };
            let blockdevs = blockdevs
                .into_iter()
                .map(|x| PathBuf::from(x.inner::<&str>().unwrap()))
                .collect::<Vec<_>>();

            let name = items.pop().ok_or_else(MethodErr::no_arg).and_then(|i| {
                i.inner::<&str>()
                    .map_err(|_| MethodErr::invalid_arg(&i))
                    .map(|i| i.to_owned())
            })?;

            let froyo = match Froyo::new(&name, &blockdevs, force) {
                Ok(x) => x,
                Err(err) => {
                    let msg = format!("Froyo create failed: {}", err);
                    return Err(MethodErr::failed(&msg));
                }
            };
            froyo.save_state().map_err(|err| {
                let msg = format!("Saving state failed: {}", err);
                MethodErr::failed(&msg)
            })?;

            let froyo = Rc::new(RefCell::new(froyo));

            let path = format!("/org/freedesktop/froyodevs/{}", froyo.borrow().id);
            let obj_path = f
                .object_path(path.clone())
                .introspectable()
                .add(froyo_interface(&froyo));

            froyo.borrow().update_dbus().map_err(|err| {
                let msg = format!("Updating DBus failed: {}", err);
                MethodErr::failed(&msg)
            })?;

            c.register_object_path(&path).map_err(|err| {
                let msg = format!("registering object path failed: {}", err);
                MethodErr::failed(&msg)
            })?;
            tree_closed_over.borrow_mut().add_o_ref(obj_path);

            let mr = m.method_return().append(path);

            froyos_closed_over.borrow_mut().push(froyo);
            Ok(vec![mr])
        })
        .in_arg(("name", "s"))
        .in_arg(("blockdevs", "as"))
        .in_arg(("force", "b"))
        .out_arg(("obj_path", "s"));

    let tree_closed_over = child_tree.clone();
    let froyos_closed_over = froyos.clone();
    let destroy_method = f
        .method("Destroy", move |m, _, _| {
            let mut items = m.get_items();
            if items.is_empty() {
                return Err(MethodErr::no_arg());
            }

            let name = items.pop().ok_or_else(MethodErr::no_arg).and_then(|i| {
                i.inner::<&str>()
                    .map_err(|_| MethodErr::invalid_arg(&i))
                    .map(|i| i.to_owned())
            })?;

            let mut froyos = froyos_closed_over.borrow_mut();
            let mut froyos_to_destroy = froyos
                .iter()
                .enumerate()
                .filter_map(|(idx, f)| {
                    if f.borrow().name == name {
                        Some((f.clone(), idx))
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>();

            let (froyo, idx) = match froyos_to_destroy.len() {
                0 => return Err(MethodErr::failed(&format!("Froyodev {} not found", name))),
                1 => froyos_to_destroy.pop().unwrap(),
                _ => {
                    return Err(MethodErr::failed(&format!(
                        "Multiple Froydevs found with name: {}. \
                          Specify froyodev uuid",
                        name
                    )))
                }
            };

            let name = format!("/org/freedesktop/froyodevs/{}", froyo.borrow().id);
            c.unregister_object_path(&name);
            tree_closed_over.borrow_mut().remove(&name.into());

            froyo.borrow_mut().destroy().map_err(|err| {
                let msg = format!("Destroying Froyodev failed: {}", err);
                MethodErr::failed(&msg)
            })?;

            froyos.remove(idx);

            Ok(vec![m.method_return()])
        })
        .in_arg(("name", "s"));

    let tree_closed_over = child_tree.clone();
    let froyos_closed_over = froyos.clone();
    let teardown_method = f
        .method("Teardown", move |m, _, _| {
            let mut items = m.get_items();
            if items.is_empty() {
                return Err(MethodErr::no_arg());
            }

            let name = items.pop().ok_or_else(MethodErr::no_arg).and_then(|i| {
                i.inner::<&str>()
                    .map_err(|_| MethodErr::invalid_arg(&i))
                    .map(|i| i.to_owned())
            })?;

            let mut froyos = froyos_closed_over.borrow_mut();
            let mut froyos_to_teardown = froyos
                .iter()
                .enumerate()
                .filter_map(|(idx, f)| {
                    if f.borrow().name == name {
                        Some((f.clone(), idx))
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>();

            let (froyo, idx) = match froyos_to_teardown.len() {
                0 => return Err(MethodErr::failed(&format!("Froyodev {} not found", name))),
                1 => froyos_to_teardown.pop().unwrap(),
                _ => {
                    return Err(MethodErr::failed(&format!(
                        "Multiple Froydevs found with name: {}. \
                          Specify froyodev uuid",
                        name
                    )))
                }
            };

            let name = format!("/org/freedesktop/froyodevs/{}", froyo.borrow().id);
            c.unregister_object_path(&name);
            tree_closed_over.borrow_mut().remove(&name.into());

            froyo.borrow_mut().teardown().map_err(|err| {
                let msg = format!("Tearing down Froyodev failed: {}", err);
                MethodErr::failed(&msg)
            })?;

            froyos.remove(idx);

            Ok(vec![m.method_return()])
        })
        .in_arg(("name", "s"));

    let obj_path = f
        .object_path("/org/freedesktop/froyo")
        .introspectable()
        .object_manager()
        .add(
            f.interface("org.freedesktop.FroyoService1")
                .add_m(create_method)
                .add_m(destroy_method)
                .add_m(teardown_method),
        );

    let base_tree = base_tree.add(obj_path);
    base_tree.set_registered(c, true)?;

    Ok(base_tree)
}

pub fn get_child_tree<'a>(
    c: &'a Connection,
    froyos: &[Rc<RefCell<Froyo<'a>>>],
) -> FroyoResult<Rc<RefCell<Tree<MethodFn<'a>>>>> {
    let f = Factory::new_fn();

    let tree = froyos.iter().fold(f.tree(), |tree, froyo| {
        let path = format!("/org/freedesktop/froyodevs/{}", froyo.borrow().id);
        let obj_path = f
            .object_path(path)
            .introspectable()
            .add(froyo_interface(froyo));
        tree.add(obj_path)
    });

    let tree = tree.add(
        f.object_path("/org/freedesktop/froyodevs")
            .introspectable()
            .object_manager(),
    );

    tree.set_registered(c, true)?;

    for froyo in &*froyos {
        froyo.borrow().update_dbus()?;
    }

    Ok(Rc::new(RefCell::new(tree)))
}
