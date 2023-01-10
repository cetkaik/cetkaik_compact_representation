 
    use super::*;

    #[test]
    fn check_initial_board() {
        let b = Board::yhuap_initial();
        assert_eq!(
            b.peek(Coord {
                row_index: 0,
                col_index: 0,
            })
            .unwrap()
            .to_string(),
            "↓黒筆"
        );
        assert_eq!(
            b.peek(Coord {
                row_index: 7,
                col_index: 1,
            })
            .unwrap()
            .to_string(),
            "↑黒弓"
        );
    }

    #[test]
    fn hop1zuo1_get_set() {
        let mut h = Hop1zuo1::new();
        let p = PieceWithSide::new(0o150).unwrap();
        h.set(p);
        assert!(h.exists(p));
        h.clear(p);
        assert!(!h.exists(p));
    }

    #[test]
    fn hop1zuo1_set_and_transmute() {
        let mut h = Hop1zuo1::new();
        h.set(PieceWithSide::new(0o133).unwrap());
        h.set(PieceWithSide::new(0o123).unwrap());
        h.set(PieceWithSide::new(0o117).unwrap());
        h.set(PieceWithSide::new(0o157).unwrap());
        h.set(PieceWithSide::new(0o132).unwrap());
        h.set(PieceWithSide::new(0o202).unwrap());
        h.set(PieceWithSide::new(0o207).unwrap());
        h.set(PieceWithSide::new(0o256).unwrap());
        h.set(PieceWithSide::new(0o245).unwrap());
        h.set(PieceWithSide::new(0o235).unwrap());
        let content = unsafe { std::mem::transmute::<Hop1zuo1, [u8; 12]>(h) };
        assert_eq!(
            content,
            [
                0b00001000, /* 兵 */ 0b00000010, /* 兵 */
                0b00000000, /* 兵 */ 0b00000001, /* 兵 */
                0b00000001, /* 弓 */ 0b00000000, /* 車 */
                0b00000101, /* 虎 */ 0b00100000, /* 馬 */
                0b00000000, /* 筆 */ 0b00100000, /* 巫 */
                0b00000000, /* 将 */ 0b00001001 /* 王と船 */
            ]
        );
    }

    #[test]
    fn hop1zuo1_transmute_and_get() {
        let mut h = unsafe {
            std::mem::transmute::<[u8; 12], Hop1zuo1>([
                0b00001000, /* 兵 */ 0b00000010, /* 兵 */
                0b00000000, /* 兵 */ 0b00000001, /* 兵 */
                0b00000001, /* 弓 */ 0b00000000, /* 車 */
                0b00000101, /* 虎 */ 0b00100000, /* 馬 */
                0b00000000, /* 筆 */ 0b00100000, /* 巫 */
                0b00000000, /* 将 */ 0b00001001, /* 王と船 */
            ])
        };

        // existence
        assert!(h.exists(PieceWithSide::new(0o133).unwrap()));
        assert!(h.exists(PieceWithSide::new(0o123).unwrap()));
        assert!(h.exists(PieceWithSide::new(0o117).unwrap()));
        assert!(h.exists(PieceWithSide::new(0o157).unwrap()));
        assert!(h.exists(PieceWithSide::new(0o132).unwrap()));
        assert!(h.exists(PieceWithSide::new(0o202).unwrap()));
        assert!(h.exists(PieceWithSide::new(0o207).unwrap()));
        assert!(h.exists(PieceWithSide::new(0o256).unwrap()));
        assert!(h.exists(PieceWithSide::new(0o245).unwrap()));
        assert!(h.exists(PieceWithSide::new(0o235).unwrap()));

        // non-existence
        assert!(!h.exists(PieceWithSide::new(0o233).unwrap()));
        assert!(!h.exists(PieceWithSide::new(0o223).unwrap()));
        assert!(!h.exists(PieceWithSide::new(0o217).unwrap()));
        assert!(!h.exists(PieceWithSide::new(0o257).unwrap()));
        assert!(!h.exists(PieceWithSide::new(0o232).unwrap()));
        assert!(!h.exists(PieceWithSide::new(0o102).unwrap()));
        assert!(!h.exists(PieceWithSide::new(0o107).unwrap()));
        assert!(!h.exists(PieceWithSide::new(0o156).unwrap()));
        assert!(!h.exists(PieceWithSide::new(0o145).unwrap()));
        assert!(!h.exists(PieceWithSide::new(0o135).unwrap()));
    }

    #[test]
    fn size() {
        assert_eq!(std::mem::size_of::<Field>(), 93);
    }

    #[allow(clippy::too_many_lines)]
    #[test]
    fn empty_sq() {
        use cetkaik_traits::IsAbsoluteBoard;
        use cetkaik_traits::IsBoard;
        assert_eq!(
            Board::yhuap_initial().empty_squares().collect::<Vec<_>>(),
            vec![
                Coord {
                    row_index: 1,
                    col_index: 2
                },
                Coord {
                    row_index: 1,
                    col_index: 4
                },
                Coord {
                    row_index: 1,
                    col_index: 6
                },
                Coord {
                    row_index: 3,
                    col_index: 0
                },
                Coord {
                    row_index: 3,
                    col_index: 1
                },
                Coord {
                    row_index: 3,
                    col_index: 2
                },
                Coord {
                    row_index: 3,
                    col_index: 3
                },
                Coord {
                    row_index: 3,
                    col_index: 4
                },
                Coord {
                    row_index: 3,
                    col_index: 5
                },
                Coord {
                    row_index: 3,
                    col_index: 6
                },
                Coord {
                    row_index: 3,
                    col_index: 7
                },
                Coord {
                    row_index: 3,
                    col_index: 8
                },
                Coord {
                    row_index: 4,
                    col_index: 0
                },
                Coord {
                    row_index: 4,
                    col_index: 1
                },
                Coord {
                    row_index: 4,
                    col_index: 2
                },
                Coord {
                    row_index: 4,
                    col_index: 3
                },
                Coord {
                    row_index: 4,
                    col_index: 5
                },
                Coord {
                    row_index: 4,
                    col_index: 6
                },
                Coord {
                    row_index: 4,
                    col_index: 7
                },
                Coord {
                    row_index: 4,
                    col_index: 8
                },
                Coord {
                    row_index: 5,
                    col_index: 0
                },
                Coord {
                    row_index: 5,
                    col_index: 1
                },
                Coord {
                    row_index: 5,
                    col_index: 2
                },
                Coord {
                    row_index: 5,
                    col_index: 3
                },
                Coord {
                    row_index: 5,
                    col_index: 4
                },
                Coord {
                    row_index: 5,
                    col_index: 5
                },
                Coord {
                    row_index: 5,
                    col_index: 6
                },
                Coord {
                    row_index: 5,
                    col_index: 7
                },
                Coord {
                    row_index: 5,
                    col_index: 8
                },
                Coord {
                    row_index: 7,
                    col_index: 2
                },
                Coord {
                    row_index: 7,
                    col_index: 4
                },
                Coord {
                    row_index: 7,
                    col_index: 6
                }
            ]
        );
    }

    #[test]
    fn hop1zuo1_of() {
        use cetkaik_fundamental::AbsoluteSide;
        use cetkaik_traits::CetkaikRepresentation;
        let field = unsafe {
            std::mem::transmute::<[u8; 93], Field>([
                162, 158, 150, 0, 173, 0, 151, 159, 163, 167, 147, 170, 155, 0, 154, 171, 146, 166,
                136, 137, 0, 139, 110, 143, 142, 141, 140, 0, 80, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 192, 0, 0, 64, 65, 66, 67, 0, 71, 70, 69, 68, 100,
                0, 0, 88, 0, 89, 0, 81, 101, 97, 93, 85, 105, 108, 104, 84, 92, 96, 0, 0, 4, 0, 0,
                0, 0, 0, 0, 0, 0, 1,
            ])
        };

        assert_eq!(
            <CetkaikCompact as CetkaikRepresentation>::hop1zuo1_of(AbsoluteSide::IASide, &field)
                .iter()
                .map(std::string::ToString::to_string)
                .collect::<Vec<_>>()
                .join(" "),
            "黒兵 赤船"
        );
        assert_eq!(
            <CetkaikCompact as CetkaikRepresentation>::hop1zuo1_of(AbsoluteSide::ASide, &field)
                .iter()
                .map(std::string::ToString::to_string)
                .collect::<Vec<_>>()
                .join(" "),
            ""
        );
    }

