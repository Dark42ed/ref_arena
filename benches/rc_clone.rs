use std::rc::Rc;

use brunch::Bench;
use ref_arena::{RcRef, RefArena};

fn seed_rc() -> Vec<Rc<i32>> {
    let mut v = Vec::new();

    for i in 0..10_000 {
        v.push(Rc::new(i));
    }

    v
}

fn seed_arena() -> (RefArena<i32>, Vec<RcRef<i32>>) {
    let mut arena = RefArena::new();
    let mut v = Vec::new();

    for i in 0..10_000 {
        v.push(arena.insert(i));
    }

    (arena, v)
}

brunch::benches! {
    Bench::new("std::rc::Rc clone 10_000")
        .run_seeded_with(seed_rc, |v| {
            let mut clones = Vec::with_capacity(10_000);

            for rc in v.iter() {
                clones.push(rc.clone());
            }

            (v, clones)
        }),

    Bench::new("RefArena clone 10_000")
        .run_seeded_with(seed_arena, |(_arena, v)| {
            let mut clones = Vec::with_capacity(10_000);

            for rc in v.iter() {
                clones.push(rc.clone());
            }

            (_arena, v, clones)
        })
}
