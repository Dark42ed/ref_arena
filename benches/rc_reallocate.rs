use brunch::Bench;
use ref_arena::{RefArena, RcRef};

fn seed_arena() -> (RefArena<i32>, Vec<RcRef<i32>>) {
    let mut arena = RefArena::new();
    let mut v = Vec::new();

    for i in 0..10_000 {
        v.push(arena.insert(i));
    }

    v.clear();

    (arena, v)
}

brunch::benches! {
    Bench::new("RefArena realloc 10_000")
        .run_seeded_with(seed_arena, |(mut arena, mut v)| {
            for i in 0..10_000 {
                v.push(arena.insert(i));
            }

            (arena, v)
        })
}
