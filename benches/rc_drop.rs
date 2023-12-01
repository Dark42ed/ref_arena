use std::rc::Rc;

use brunch::Bench;
use rcarena::{RcArena, RcRef};

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
    Bench::new("Rc drop 10_000")
        .run_seeded_with(seed_rc, |v| {
            drop(v)
        }),
    
    Bench::new("RcArena drop 10_000")
        .run_seeded_with(seed_arena, |(arena, v)| {
            drop((arena, v))
        })
}