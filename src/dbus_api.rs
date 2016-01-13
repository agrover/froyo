// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use dbus::{Connection, NameFlag};
use dbus::tree::Factory;
use dbus::tree::Tree;
use dbus::tree::MethodFn;

use froyo::Froyo;
use types::FroyoResult;

pub fn get_tree<'a>(c: &Connection, froyos: &[Froyo])
                       -> FroyoResult<Tree<MethodFn<'a>>> {
    c.register_name("org.freedesktop.Froyo1", NameFlag::ReplaceExisting as u32).unwrap();

    let f = Factory::new_fn();
    let tree = froyos
        .iter()
        .fold(f.tree(), |tree, froyo| {
            tree.add(f.object_path(format!("/org/freedesktop/froyo/devs/{}", froyo.name))
                     .introspectable()
                     .object_manager()
                     .add(f.interface("org.freedesktop.FroyoService1")
                          .add_m(f.method("Capacity", |m,_,_| {
                              let s = vec![1u64.into(), 3u64.into(), 5u64.into(), 7u64.into()];
                              let mut mr = m.method_return();
                              mr.append_items(&*s);
                              Ok(vec![mr])
                          }).out_arg(("reply", "(tttt)")))
                          .add_m(f.method("Status", |m,_,_| {
                              Ok(vec![m.method_return()
                                  .append(0)
                                  .append(1)
                                  .append("good")
                                  .append(4)
                                  .append("great")])
                          }).out_arg(("reply", "(uusus)")))
                          )
                     )
        });

    Ok(tree)
}

