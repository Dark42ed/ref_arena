use std::{rc::Rc, hint::black_box};

use brunch::Bench;
use rc_arena::{RcArena, RcRef};

fn seed_rc() -> Vec<Rc<i32>> {
    let mut v = Vec::new();

    for i in 0..10_000 {
        v.push(Rc::new(i));
    }

    v
}

fn seed_arena() -> (RcArena<i32>, Vec<RcRef<i32>>) {
    let mut arena = RcArena::new();
    let mut v = Vec::new();

    for i in 0..10_000 {
        v.push(arena.insert(i));
    }

    (arena, v)
}

brunch::benches! {
    Bench::new("Rc deref 10_000")
        .run_seeded_with(seed_rc, |v| {
            for rc in v.iter() {
                black_box(**rc);
            }

            v
        }),
    
    Bench::new("RcArena deref 10_000")
        .run_seeded_with(seed_arena, |(_arena, v)| {
            for rc in v.iter() {
                black_box(**rc);
            }

            (_arena, v)
        })
}