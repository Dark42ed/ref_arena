use ref_arena::{bounded::BoundedRefArena, RefArena};

#[test]
pub fn i32_100() {
    let mut arena = RefArena::new();
    let mut refs = Vec::new();

    for i in 0..100 {
        refs.push(arena.insert(i));
    }

    for (i, rc) in refs.iter().enumerate() {
        assert_eq!(**rc, i);
        assert_eq!(*rc.clone(), i);
    }
}


#[test]
pub fn i32_100_bounded() {
    let arena = BoundedRefArena::new();
    let mut refs = Vec::new();

    for i in 0..100 {
        refs.push(arena.insert(i));
    }

    for (i, rc) in refs.iter().enumerate() {
        assert_eq!(**rc, i);
        assert_eq!(*rc.clone(), i);
    }
}