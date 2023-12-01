use std::rc::Rc;

use brunch::Bench;
use rcarena::{RcArena, RcRef};

fn seed_rc() -> Vec<Rc<i32>> {
    Vec::with_capacity(10_000)
}

fn seed_arena() -> (RcArena<i32>, Vec<RcRef<i32>>) {
    (
        RcArena::new(),
        Vec::new()
    )
}

brunch::benches! {
    Bench::new("Rc allocate 10_000")
        .run_seeded_with(seed_rc, |mut v| {
            for i in 0..10_000 {
                v.push(Rc::new(i));
            }

            v
        }),
    
    Bench::new("RcArena allocate 10_000")
        .run_seeded_with(seed_arena, |(mut arena, mut v)| {
            for i in 0..10_000 {
                v.push(arena.insert(i));
            }

            v
        })
}