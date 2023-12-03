use std::{hint::black_box, rc::Rc};

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
    Bench::new("std::rc::Rc deref 10_000")
        .run_seeded_with(seed_rc, |v| {
            for rc in v.iter() {
                black_box(**rc);
            }

            v
        }),

    Bench::new("RefArena deref 10_000")
        .run_seeded_with(seed_arena, |(_arena, v)| {
            for rc in v.iter() {
                black_box(**rc);
            }

            (_arena, v)
        })
}
