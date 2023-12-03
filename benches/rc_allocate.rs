use std::rc::Rc;

use brunch::Bench;
use ref_arena::{RcRef, RefArena};

fn seed_rc() -> Vec<Rc<i32>> {
    Vec::with_capacity(10_000)
}

fn seed_arena() -> (RefArena<i32>, Vec<RcRef<i32>>) {
    (RefArena::new(), Vec::new())
}

brunch::benches! {
    Bench::new("std::rc::Rc allocate 10_000")
        .run_seeded_with(seed_rc, |mut v| {
            for i in 0..10_000 {
                v.push(Rc::new(i));
            }

            v
        }),

    Bench::new("RefArena allocate 10_000")
        .run_seeded_with(seed_arena, |(mut arena, mut v)| {
            for i in 0..10_000 {
                v.push(arena.insert(i));
            }

            v
        })
}
