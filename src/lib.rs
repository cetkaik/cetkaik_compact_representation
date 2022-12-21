#![warn(clippy::pedantic, clippy::nursery)]
#![allow(clippy::unreadable_literal)]
use std::num::NonZeroU8;

use cetkaik_core::{
    absolute, serialize_color, Color, IsAbsoluteBoard, IsBoard, IsField, Profession,
};

#[derive(Copy, Clone, Debug)]
pub struct Board([SingleRow; 9]);

impl Board {
    /// # Example
    /// ```
    /// use cetkaik_compact_representation::*;
    /// use cetkaik_core::IsAbsoluteBoard;
    /// assert_eq!(
    ///     Board::yhuap_initial().both_side_and_tam().map(|(coord, piece)| piece).collect::<Vec<_>>(),
    ///     [0o242, 0o236, 0o226, 0o252, 0o255, 0o253, 0o227,
    ///      0o237, 0o243, 0o247, 0o223, 0o233, 0o232, 0o222,
    ///      0o246, 0o210, 0o211, 0o212, 0o213, 0o257, 0o217,
    ///      0o216, 0o215, 0o214, 0o300, 0o100, 0o101, 0o102,
    ///      0o103, 0o156, 0o107, 0o106, 0o105, 0o104, 0o144,
    ///      0o120, 0o130, 0o131, 0o121, 0o145, 0o141, 0o135,
    ///      0o125, 0o151, 0o154, 0o150, 0o124, 0o134, 0o140].into_iter().map(|c| PieceWithSide::new(c).unwrap()).collect::<Vec<_>>()
    /// )
    /// ```
    pub fn both_side_and_tam(self) -> impl Iterator<Item = (Coord, PieceWithSide)> {
        self.0.into_iter().enumerate().flat_map(|(row_index, row)| {
            row.into_iter()
                .enumerate()
                .filter_map(move |(col_index, maybe_piece)| {
                    match (maybe_piece, Coord::new(row_index, col_index)) {
                        (Some(piece), Some(coord)) => Some((coord, piece)),
                        _ => None,
                    }
                })
        })
    }

    /// # Example
    /// ```
    /// use cetkaik_compact_representation::*;
    /// use cetkaik_core::IsAbsoluteBoard;
    /// assert_eq!(
    ///     Board::yhuap_initial().ia_side_and_tam().map(|(coord, piece)| piece).collect::<Vec<_>>(),
    ///     [0o300, 0o100, 0o101, 0o102,
    ///      0o103, 0o156, 0o107, 0o106, 0o105, 0o104, 0o144,
    ///      0o120, 0o130, 0o131, 0o121, 0o145, 0o141, 0o135,
    ///      0o125, 0o151, 0o154, 0o150, 0o124, 0o134, 0o140].into_iter().map(|c| PieceWithSide::new(c).unwrap()).collect::<Vec<_>>()
    /// )
    /// ```
    pub fn ia_side_and_tam(self) -> impl Iterator<Item = (Coord, PieceWithSide)> {
        self.0.into_iter().enumerate().flat_map(|(row_index, row)| {
            row.into_iter()
                .enumerate()
                .filter_map(move |(col_index, maybe_piece)| {
                    match (maybe_piece, Coord::new(row_index, col_index)) {
                        (Some(piece), Some(coord)) if (piece.0.get() & 0o100) != 0 => {
                            Some((coord, piece))
                        }
                        _ => None,
                    }
                })
        })
    }

    /// # Example
    /// ```
    /// use cetkaik_compact_representation::*;
    /// use cetkaik_core::IsAbsoluteBoard;
    /// assert_eq!(
    ///     Board::yhuap_initial().a_side_and_tam().map(|(coord, piece)| piece).collect::<Vec<_>>(),
    ///     [0o242, 0o236, 0o226, 0o252, 0o255, 0o253, 0o227,
    ///      0o237, 0o243, 0o247, 0o223, 0o233, 0o232, 0o222,
    ///      0o246, 0o210, 0o211, 0o212, 0o213, 0o257, 0o217,
    ///      0o216, 0o215, 0o214, 0o300].into_iter().map(|c| PieceWithSide::new(c).unwrap()).collect::<Vec<_>>()
    /// )
    /// ```
    pub fn a_side_and_tam(self) -> impl Iterator<Item = (Coord, PieceWithSide)> {
        self.0.into_iter().enumerate().flat_map(|(row_index, row)| {
            row.into_iter()
                .enumerate()
                .filter_map(move |(col_index, maybe_piece)| {
                    match (maybe_piece, Coord::new(row_index, col_index)) {
                        (Some(piece), Some(coord)) if (piece.0.get() & 0o200) != 0 => {
                            Some((coord, piece))
                        }
                        _ => None,
                    }
                })
        })
    }
}

type SingleRow = [Option<PieceWithSide>; 9];

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct PieceWithSide(NonZeroU8);

pub enum MaybeTam2<T> {
    Tam2,
    NotTam2(T),
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum Perspective {
    /// IA is the lowermost row;
    /// the player who had occupied the IA row in the beginning of the game has pieces that point upward
    /// (i.e. you)
    /// ／IAは一番下の行であり、初期状態でIA行を占有していたプレイヤーは駒が上向き（=あなた）である。
    IaIsDownAndPointsUpward,
}

impl PieceWithSide {
    #[must_use]
    pub fn new(u: u8) -> Option<Self> {
        if (0o100..=0o157).contains(&u) || (0o200..=0o257).contains(&u) || 0o300 == u {
            let u = std::num::NonZeroU8::new(u)?;
            Some(Self(u))
        } else {
            None
        }
    }
    /// # Safety
    /// The input must satisfy `(0o100..=0o157).contains(&u) || (0o200..=0o257).contains(&u) || 0o300 == u`
    #[must_use]
    pub const unsafe fn new_unchecked(u: u8) -> Self {
        Self(std::num::NonZeroU8::new_unchecked(u))
    }
    #[must_use]
    pub const fn color(self) -> Color {
        if self.0.get() % 2 == 0 {
            Color::Huok2
        } else {
            Color::Kok1
        }
    }
    #[must_use]
    pub const fn is_tam2(self) -> bool {
        self.0.get() & 0o300 == 0o300
    }
    #[must_use]
    pub const fn prof(self) -> MaybeTam2<Profession> {
        if self.is_tam2() {
            return MaybeTam2::Tam2;
        }
        let p = self.0.get() & 0o77;
        if p < 16 {
            MaybeTam2::NotTam2(Profession::Kauk2)
        } else if p < 20 {
            MaybeTam2::NotTam2(Profession::Gua2)
        } else if p < 24 {
            MaybeTam2::NotTam2(Profession::Kaun1)
        } else if p < 28 {
            MaybeTam2::NotTam2(Profession::Dau2)
        } else if p < 32 {
            MaybeTam2::NotTam2(Profession::Maun1)
        } else if p < 36 {
            MaybeTam2::NotTam2(Profession::Kua2)
        } else if p < 40 {
            MaybeTam2::NotTam2(Profession::Tuk2)
        } else if p < 44 {
            MaybeTam2::NotTam2(Profession::Uai1)
        } else if p < 46 {
            MaybeTam2::NotTam2(Profession::Io)
        } else {
            MaybeTam2::NotTam2(Profession::Nuak1)
        }
    }
    #[must_use]
    pub const fn prof_and_side(self) -> MaybeTam2<(Profession, absolute::Side)> {
        if self.is_tam2() {
            return MaybeTam2::Tam2;
        }
        let s = if self.0.get() & 0o300 == 0o100 {
            absolute::Side::IASide
        } else {
            absolute::Side::ASide
        };
        let p = self.0.get() & 0o77;
        if p < 16 {
            MaybeTam2::NotTam2((Profession::Kauk2, s))
        } else if p < 20 {
            MaybeTam2::NotTam2((Profession::Gua2, s))
        } else if p < 24 {
            MaybeTam2::NotTam2((Profession::Kaun1, s))
        } else if p < 28 {
            MaybeTam2::NotTam2((Profession::Dau2, s))
        } else if p < 32 {
            MaybeTam2::NotTam2((Profession::Maun1, s))
        } else if p < 36 {
            MaybeTam2::NotTam2((Profession::Kua2, s))
        } else if p < 40 {
            MaybeTam2::NotTam2((Profession::Tuk2, s))
        } else if p < 44 {
            MaybeTam2::NotTam2((Profession::Uai1, s))
        } else if p < 46 {
            MaybeTam2::NotTam2((Profession::Io, s))
        } else {
            MaybeTam2::NotTam2((Profession::Nuak1, s))
        }
    }

    /// Inverts side, but also swaps an empty space into Tam2 and vice versa
    ///
    /// ```
    /// use cetkaik_compact_representation::PieceWithSide;
    /// let a = PieceWithSide::new(0o240).unwrap();
    /// assert_eq!(a.to_string(), "↓黒筆");
    /// let b = PieceWithSide::invert_side_with_tam_entangled(Some(a)).unwrap();
    /// assert_eq!(b.to_string(), "↑黒筆");
    ///
    /// let c = PieceWithSide::invert_side_with_tam_entangled(None).unwrap();
    /// assert_eq!(c.to_string(), "皇");
    /// ```
    #[must_use]
    pub const fn invert_side_with_tam_entangled(s: Option<Self>) -> Option<Self> {
        unsafe {
            let s = std::mem::transmute::<Option<Self>, u8>(s);
            std::mem::transmute::<u8, Option<Self>>(s ^ 0o300)
        }
    }

    /// Inverts side; returns `None` if `Tam2` is passed.
    /// ```
    /// use cetkaik_compact_representation::PieceWithSide;
    /// let a = PieceWithSide::new(0o240).unwrap();
    /// assert_eq!(a.to_string(), "↓黒筆");
    /// let b = a.invert_side().unwrap();
    /// assert_eq!(b.to_string(), "↑黒筆");
    ///
    /// let c = PieceWithSide::new(0o300).unwrap();
    /// assert_eq!(c.to_string(), "皇");
    /// assert_eq!(c.invert_side(), None);
    /// ```
    #[must_use]
    pub const fn invert_side(self) -> Option<Self> {
        Self::invert_side_with_tam_entangled(Some(self))
    }

    /// Checks whether `self` can take `other`.
    /// That is, either
    /// - `self`'s top two bits are `10` and `other`'s top two bits are `01`
    /// - vice versa
    /// ```
    /// use cetkaik_compact_representation::PieceWithSide;
    /// let a = PieceWithSide::new(0o240).unwrap();
    /// assert_eq!(a.to_string(), "↓黒筆");
    /// let b = PieceWithSide::new(0o140).unwrap();
    /// assert_eq!(b.to_string(), "↑黒筆");
    /// let tam = PieceWithSide::new(0o300).unwrap();
    /// assert_eq!(tam.to_string(), "皇");
    ///
    /// assert!(a.can_take(b));
    /// assert!(b.can_take(a));
    /// assert!(!a.can_take(a));
    /// assert!(!b.can_take(b));
    /// assert!(!a.can_take(tam));
    /// assert!(!b.can_take(tam));
    /// ```
    #[must_use]
    pub const fn can_take(self, other: Self) -> bool {
        // 10xxxxxx
        // 01xxxxxx
        //--------- xor
        // 11xxxxxx
        //--------- not
        // 00xxxxxx
        // 11000000
        //---------and
        // 00000000
        0o300 & !(self.0.get() ^ other.0.get()) == 0
    }


    /// ```
    /// use cetkaik_compact_representation::PieceWithSide;
    /// let a = PieceWithSide::new(0o240).unwrap();
    /// assert_eq!(a.to_string(), "↓黒筆");
    /// let b = PieceWithSide::new(0o140).unwrap();
    /// assert_eq!(b.to_string(), "↑黒筆");
    /// let tam = PieceWithSide::new(0o300).unwrap();
    /// assert_eq!(tam.to_string(), "皇");
    ///
    /// assert!(a.can_be_moved_by(cetkaik_core::absolute::Side::ASide));
    /// assert!(!b.can_be_moved_by(cetkaik_core::absolute::Side::ASide));
    /// assert!(tam.can_be_moved_by(cetkaik_core::absolute::Side::ASide));
    /// assert!(b.can_be_moved_by(cetkaik_core::absolute::Side::IASide));
    /// assert!(!a.can_be_moved_by(cetkaik_core::absolute::Side::IASide));
    /// assert!(tam.can_be_moved_by(cetkaik_core::absolute::Side::IASide));
    /// ```
    #[must_use]
    pub const fn can_be_moved_by(self, side: cetkaik_core::absolute::Side) -> bool {
        let u8 = self.0.get();
        match side {
            absolute::Side::ASide => 0o200 & u8 != 0,
            absolute::Side::IASide => 0o100 & u8 != 0,
        }
    }
}

impl std::fmt::Display for PieceWithSide {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use cetkaik_core::serialize_prof;
        match self.prof_and_side() {
            MaybeTam2::Tam2 => write!(f, "皇"),
            MaybeTam2::NotTam2((prof, side)) => write!(
                f,
                "{}{}{}",
                if side == absolute::Side::IASide {
                    "↑"
                } else {
                    "↓"
                },
                serialize_color(self.color()),
                serialize_prof(prof)
            ),
        }
    }
}

/// どちらにも属していなければ 0 を、IA 側は 1 を、A 側は 2 を
#[derive(Copy, Clone, Debug)]
pub struct Hop1zuo1([u8; 12]);

#[derive(Copy, Clone, Debug)]
pub struct Field {
    board: Board,
    hop1zuo1: Hop1zuo1,
}

impl IsField for Field {
    type Board = Board;
    type Coord = Coord;
    type PieceWithSide = PieceWithSide;
    type Side = cetkaik_core::absolute::Side;

    fn move_nontam_piece_from_src_to_dest_while_taking_opponent_piece_if_needed(
        &self,
        from: Self::Coord,
        to: Self::Coord,
        whose_turn: Self::Side,
    ) -> Result<Self, &'static str>
    where
        Self: std::marker::Sized,
    {
        let Some(src_piece) = self.as_board().0[from.row_index as usize][from.col_index as usize]
        else { return Err("src does not contain a piece") };

        if !src_piece.can_be_moved_by(whose_turn) {
            return Err("not the right owner")
        }
        
        match self.as_board().0[to.row_index as usize][to.col_index as usize] {
            None => {
                let mut new_self = *self;
                new_self.as_board_mut().mov(from, to);
                Ok(new_self)
            }
            Some(_) => self.try_take(from, to).ok_or("cannot take"),
        }
    }

    /// # Panics
    /// Panics when `p` is not in hop1zuo1.
    fn parachute_nontam(&mut self, p: PieceWithSide, to: Coord) {
        assert!(
            self.hop1zuo1.exists(p),
            "cannot place {:?} ({}) because it is not in hop1zuo1",
            p,
            p
        );

        self.hop1zuo1.clear(p);
        self.board.put(to, Some(p));
    }

    fn as_board(&self) -> &Board {
        &self.board
    }

    fn as_board_mut(&mut self) -> &mut Board {
        &mut self.board
    }
}

impl Field {
    /// # Panics
    /// Panics when:
    /// - `from == to`
    /// - either `from` or `to` is an empty square
    /// - either `from` or `to` is tam2
    pub fn take(&mut self, from: Coord, to: Coord) {
        assert_ne!(from, to);
        match (self.board.pop(from), self.board.pop(to)) {
            (Some(src_piece), Some(dst_piece)) => {
                assert!(src_piece.can_take(dst_piece));
                self.board.put(to, Some(src_piece));
                self.hop1zuo1
                    .set(dst_piece.invert_side().expect("Cannot take Tam2"));
            }
            _ => panic!("Empty square encountered at either {from:?} or {to:?}"),
        }
    }

    /// # Error
    /// Gives an error when:
    /// - `from == to`
    /// - either `from` or `to` is an empty square
    /// - either `from` or `to` is tam2
    #[must_use]
    pub fn try_take(&self, from: Coord, to: Coord) -> Option<Self> {
        let mut new_self = *self;
        match (new_self.board.pop(from), new_self.board.pop(to)) {
            (Some(src_piece), Some(dst_piece)) => {
                if !src_piece.can_take(dst_piece) {
                    return None;
                }
                new_self.board.put(to, Some(src_piece));
                new_self
                    .hop1zuo1
                    .set(dst_piece.invert_side().expect("Cannot take Tam2"));
                Some(new_self)
            }
            _ => None,
        }
    }

    #[must_use]
    pub const fn to_hop1zuo1(self) -> Hop1zuo1 {
        self.hop1zuo1
    }

    #[must_use]
    pub const fn as_hop1zuo1(&self) -> &Hop1zuo1 {
        &self.hop1zuo1
    }

    #[must_use]
    pub fn as_hop1zuo1_mut(&mut self) -> &mut Hop1zuo1 {
        &mut self.hop1zuo1
    }

    #[must_use]
    pub const fn to_board(self) -> Board {
        self.board
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct Coord {
    row_index: u8,
    col_index: u8,
}

impl Coord {
    #[must_use]
    #[allow(clippy::cast_possible_truncation)]
    pub const fn new(row_index: usize, col_index: usize) -> Option<Self> {
        if row_index < 9 && col_index < 9 {
            Some(Self {
                row_index: row_index as u8,
                col_index: col_index as u8,
            })
        } else {
            None
        }
    }
    #[must_use]
    pub const fn add_delta(self, row_delta: isize, col_delta: isize) -> Option<Self> {
        const fn add(base: usize, delta: isize) -> Option<usize> {
            match base.checked_add_signed(delta) {
                a @ Some(0..=8) => a,
                _ => None,
            }
        }
        //  `?` is not allowed in a `const fn`
        match (
            add(self.row_index as usize, row_delta),
            add(self.col_index as usize, col_delta),
        ) {
            (Some(row_index), Some(col_index)) => Self::new(row_index, col_index),
            _ => None,
        }
    }
    #[must_use]
    pub fn is_tam_hue_by_default(coord: Self) -> bool {
        coord
            == Self {
                row_index: 2,
                col_index: 2,
            }
            || coord
                == Self {
                    row_index: 2,
                    col_index: 6,
                }
            || coord
                == Self {
                    row_index: 3,
                    col_index: 3,
                }
            || coord
                == Self {
                    row_index: 3,
                    col_index: 5,
                }
            || coord
                == Self {
                    row_index: 4,
                    col_index: 4,
                }
            || coord
                == Self {
                    row_index: 5,
                    col_index: 3,
                }
            || coord
                == Self {
                    row_index: 5,
                    col_index: 5,
                }
            || coord
                == Self {
                    row_index: 6,
                    col_index: 2,
                }
            || coord
                == Self {
                    row_index: 6,
                    col_index: 6,
                }
    }

    #[allow(clippy::nonminimal_bool)]
    #[must_use]
    pub const fn is_water(coord: Self) -> bool {
        let Self {
            row_index: row,
            col_index: col,
        } = coord;
        (row == 4 && col == 2)
            || (row == 4 && col == 3)
            || (row == 4 && col == 4)
            || (row == 4 && col == 5)
            || (row == 4 && col == 6)
            || (row == 2 && col == 4)
            || (row == 3 && col == 4)
            || (row == 5 && col == 4)
            || (row == 6 && col == 4)
    }

    #[must_use]
    pub fn distance(a: Self, b: Self) -> i32 {
        let Self {
            row_index: x1,
            col_index: y1,
        } = a;
        let Self {
            row_index: x2,
            col_index: y2,
        } = b;

        let x_distance = (i32::from(x1) - i32::from(x2)).abs();
        let y_distance = (i32::from(y1) - i32::from(y2)).abs();

        x_distance.max(y_distance)
    }

    #[must_use]
    pub const fn same_direction(origin: Self, a: Self, b: Self) -> bool {
        let a_u = (a.row_index as i32) - (origin.row_index as i32);
        let a_v = (a.col_index as i32) - (origin.col_index as i32);
        let b_u = (b.row_index as i32) - (origin.row_index as i32);
        let b_v = (b.col_index as i32) - (origin.col_index as i32);

        (a_u * b_u + a_v * b_v > 0) && (a_u * b_v - a_v * b_u == 0)
    }
}

impl std::fmt::Display for Coord {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let col = if self.col_index == 0 {
            'K'
        } else if self.col_index == 1 {
            'L'
        } else if self.col_index == 2 {
            'N'
        } else if self.col_index == 3 {
            'T'
        } else if self.col_index == 4 {
            'Z'
        } else if self.col_index == 5 {
            'X'
        } else if self.col_index == 6 {
            'C'
        } else if self.col_index == 7 {
            'M'
        } else {
            'P'
        };

        let row = if self.row_index == 0 {
            "A"
        } else if self.row_index == 1 {
            "E"
        } else if self.row_index == 2 {
            "I"
        } else if self.row_index == 3 {
            "U"
        } else if self.row_index == 4 {
            "O"
        } else if self.row_index == 5 {
            "Y"
        } else if self.row_index == 6 {
            "AI"
        } else if self.row_index == 7 {
            "AU"
        } else {
            "IA"
        };
        write!(f, "{col}{row}")
    }
}

impl IsAbsoluteBoard for Board {
    #[must_use]
    fn yhuap_initial() -> Self {
        Self(unsafe {
            std::mem::transmute::<[[u8; 9]; 9], [SingleRow; 9]>([
                [
                    0o242, 0o236, 0o226, 0o252, 0o255, 0o253, 0o227, 0o237, 0o243,
                ],
                [
                    0o247, 0o223, 0o000, 0o233, 0o000, 0o232, 0o000, 0o222, 0o246,
                ],
                [
                    0o210, 0o211, 0o212, 0o213, 0o257, 0o217, 0o216, 0o215, 0o214,
                ],
                [
                    0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000,
                ],
                [
                    0o000, 0o000, 0o000, 0o000, 0o300, 0o000, 0o000, 0o000, 0o000,
                ],
                [
                    0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000,
                ],
                [
                    0o100, 0o101, 0o102, 0o103, 0o156, 0o107, 0o106, 0o105, 0o104,
                ],
                [
                    0o144, 0o120, 0o000, 0o130, 0o000, 0o131, 0o000, 0o121, 0o145,
                ],
                [
                    0o141, 0o135, 0o125, 0o151, 0o154, 0o150, 0o124, 0o134, 0o140,
                ],
            ])
        })
    }
}

impl IsBoard for Board {
    type PieceWithSide = PieceWithSide;
    type Coord = Coord;

    fn peek(&self, c: Coord) -> Option<PieceWithSide> {
        self.0[c.row_index as usize][c.col_index as usize]
    }
    fn pop(&mut self, c: Coord) -> Option<PieceWithSide> {
        let p = self.0[c.row_index as usize][c.col_index as usize];
        self.put(c, None);
        p
    }
    fn put(&mut self, c: Coord, p: Option<PieceWithSide>) {
        self.0[c.row_index as usize][c.col_index as usize] = p;
    }

    /// # Panics
    /// Panics if the coordinate given by `c` is empty.
    fn assert_empty(&self, c: Coord) {
        assert!(
            self.peek(c).is_none(),
            "Expected the square {:?} to be empty, but it was occupied",
            c
        );
    }

    /// # Panics
    /// Panics if the coordinate given by `c` is occupied.
    fn assert_occupied(&self, c: Coord) {
        assert!(
            self.peek(c).is_some(),
            "Expected the square {:?} to be occupied, but it was empty",
            c
        );
    }
}

impl Default for Hop1zuo1 {
    fn default() -> Self {
        Self::new()
    }
}

impl Hop1zuo1 {
    #[must_use]
    pub const fn new() -> Self {
        Self([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0])
    }
    /// # Panics
    /// - putting Tam2 into hop1zuo1
    /// - putting a piece that is already in hop1zuo1
    pub fn set(&mut self, p: PieceWithSide) {
        let u8 = p.0.get();
        assert!((0o100..=0o157).contains(&u8) || (0o200..=0o257).contains(&u8) || 0o300 == u8);
        assert!(u8 != 0o300, "Tried to set Tam2 into Hop1zuo1");
        assert!(!self.exists(p));
        assert!(!self.exists(p.invert_side().unwrap()));
        unsafe { self.set_unchecked(p) }
    }

    /// # Safety
    /// assumes that `p` is valid and not Tam2
    pub unsafe fn set_unchecked(&mut self, p: PieceWithSide) {
        let u8 = p.0.get();

        // M      L
        // xxxxxxxx
        // SdIndxBp
        let side = u8 >> 6;
        let index = (u8 & 0o77) >> 2;
        let bit_position = 6 - (u8 & 0o03) * 2;

        *self.0.get_unchecked_mut(index as usize) |= side << bit_position;
    }

    /// # Panics
    /// Panics when either:
    /// - `p` is Tam2
    /// - `p` does not exist in hop1zuo1
    pub fn clear(&mut self, p: PieceWithSide) {
        let u8 = p.0.get();

        assert!((0o100..=0o157).contains(&u8) || (0o200..=0o257).contains(&u8) || 0o300 == u8);
        assert!(u8 != 0o300, "Tried to set Tam2 into Hop1zuo1");
        assert!(self.exists(p));
        unsafe { self.clear_unchecked(p) }
    }

    /// # Safety
    /// assumes that `p` is valid and not Tam2
    pub unsafe fn clear_unchecked(&mut self, p: PieceWithSide) {
        let u8 = p.0.get();

        // M      L
        // xxxxxxxx
        // SdIndxBp
        // let _side = u8 >> 6;
        let index = (u8 & 0o77) >> 2;
        let bit_position = 6 - (u8 & 0o03) * 2;

        *self.0.get_unchecked_mut(index as usize) &= !(3 << bit_position);
    }
    /// # Panics
    /// - p is Tam2
    pub fn exists(&mut self, p: PieceWithSide) -> bool {
        let u8 = p.0.get();
        assert!((0o100..=0o157).contains(&u8) || (0o200..=0o257).contains(&u8) || 0o300 == u8);
        assert!(u8 != 0o300, "Tried to ask wheter Tam2 exists in Hop1zuo1");
        unsafe { self.exists_unchecked(p) }
    }

    /// # Safety
    /// assumes that `p` is valid and not Tam2
    #[must_use]
    pub unsafe fn exists_unchecked(&self, p: PieceWithSide) -> bool {
        let u8 = p.0.get();

        // M      L
        // xxxxxxxx
        // SdIndxBp
        let side = u8 >> 6;
        let index = (u8 & 0o77) >> 2;
        let bit_position = 6 - (u8 & 0o03) * 2;
        0 != (*self.0.get_unchecked(index as usize) & (side << bit_position))
    }

    /// Guaranteed to output the result so that the lower 6 bits are in the ascending order.
    /// # Example
    /// ```
    /// use cetkaik_compact_representation::*;
    /// let h = unsafe {
    ///       std::mem::transmute::<[u8; 12], Hop1zuo1>([
    ///           0b00001000, /* 兵 */ 0b00000010, /* 兵 */
    ///           0b00000000, /* 兵 */ 0b00000001, /* 兵 */
    ///           0b00000001, /* 弓 */ 0b00000000, /* 車 */
    ///           0b00000101, /* 虎 */ 0b00100000, /* 馬 */
    ///           0b00000000, /* 筆 */ 0b00100000, /* 巫 */
    ///           0b00000000, /* 将 */ 0b00001001, /* 王と船 */
    ///       ])
    ///   };
    ///
    ///   assert_eq!(
    ///       h.both_hop1zuo1().collect::<Vec<_>>(),
    ///       vec![
    ///           PieceWithSide::new(0o202).unwrap(),
    ///           PieceWithSide::new(0o207).unwrap(),
    ///           PieceWithSide::new(0o117).unwrap(),
    ///           PieceWithSide::new(0o123).unwrap(),
    ///           PieceWithSide::new(0o132).unwrap(),
    ///           PieceWithSide::new(0o133).unwrap(),
    ///           PieceWithSide::new(0o235).unwrap(),
    ///           PieceWithSide::new(0o245).unwrap(),
    ///           PieceWithSide::new(0o256).unwrap(),
    ///           PieceWithSide::new(0o157).unwrap(),
    ///       ]
    ///   )
    /// ```
    pub fn both_hop1zuo1(self) -> impl Iterator<Item = PieceWithSide> {
        BothHop1Zuo1Iter { h: self.0, i: 0 }
    }

    /// Guaranteed to output the result so that the lower 6 bits are in the ascending order.
    /// # Example
    /// ```
    /// use cetkaik_compact_representation::*;
    /// let h = unsafe {
    ///       std::mem::transmute::<[u8; 12], Hop1zuo1>([
    ///           0b00001000, /* 兵 */ 0b00000010, /* 兵 */
    ///           0b00000000, /* 兵 */ 0b00000001, /* 兵 */
    ///           0b00000001, /* 弓 */ 0b00000000, /* 車 */
    ///           0b00000101, /* 虎 */ 0b00100000, /* 馬 */
    ///           0b00000000, /* 筆 */ 0b00100000, /* 巫 */
    ///           0b00000000, /* 将 */ 0b00001001, /* 王と船 */
    ///       ])
    ///   };
    ///
    ///   assert_eq!(
    ///       h.ia_side_hop1zuo1().collect::<Vec<_>>(),
    ///       vec![
    ///           PieceWithSide::new(0o117).unwrap(),
    ///           PieceWithSide::new(0o123).unwrap(),
    ///           PieceWithSide::new(0o132).unwrap(),
    ///           PieceWithSide::new(0o133).unwrap(),
    ///           PieceWithSide::new(0o157).unwrap(),
    ///       ]
    ///   )
    /// ```
    pub fn ia_side_hop1zuo1(self) -> impl Iterator<Item = PieceWithSide> {
        IASideHop1Zuo1Iter { h: self.0, i: 0 }
    }

    /// # Example
    /// ```
    /// use cetkaik_compact_representation::*;
    /// use cetkaik_core::{Color, Profession};
    /// let h = unsafe {
    ///       std::mem::transmute::<[u8; 12], Hop1zuo1>([
    ///           0b00001000, /* 兵 */ 0b00000010, /* 兵 */
    ///           0b00000000, /* 兵 */ 0b00000001, /* 兵 */
    ///           0b00000001, /* 弓 */ 0b00000000, /* 車 */
    ///           0b00000101, /* 虎 */ 0b00100000, /* 馬 */
    ///           0b00000000, /* 筆 */ 0b00100000, /* 巫 */
    ///           0b00000000, /* 将 */ 0b00001001, /* 王と船 */
    ///       ])
    ///   };
    ///
    ///   assert_eq!(
    ///       h.ia_side_hop1zuo1_color_and_prof().map(|cp| cp.to_string()).collect::<Vec<_>>().join(" "),
    ///       "赤兵 赤弓 黒虎 赤虎 赤船".to_string()
    ///   )
    /// ```
    pub fn ia_side_hop1zuo1_color_and_prof(
        self,
    ) -> impl Iterator<Item = cetkaik_core::ColorAndProf> {
        IASideHop1Zuo1IterWithColorAndProf { h: self.0, i: 0 }
    }

    /// Guaranteed to output the result so that the lower 6 bits are in the ascending order.
    /// # Example
    /// ```
    /// use cetkaik_compact_representation::*;
    /// let h = unsafe {
    ///       std::mem::transmute::<[u8; 12], Hop1zuo1>([
    ///           0b00001000, /* 兵 */ 0b00000010, /* 兵 */
    ///           0b00000000, /* 兵 */ 0b00000001, /* 兵 */
    ///           0b00000001, /* 弓 */ 0b00000000, /* 車 */
    ///           0b00000101, /* 虎 */ 0b00100000, /* 馬 */
    ///           0b00000000, /* 筆 */ 0b00100000, /* 巫 */
    ///           0b00000000, /* 将 */ 0b00001001, /* 王と船 */
    ///       ])
    ///   };
    ///
    ///   assert_eq!(
    ///       h.a_side_hop1zuo1().collect::<Vec<_>>(),
    ///       vec![
    ///           PieceWithSide::new(0o202).unwrap(),
    ///           PieceWithSide::new(0o207).unwrap(),
    ///           PieceWithSide::new(0o235).unwrap(),
    ///           PieceWithSide::new(0o245).unwrap(),
    ///           PieceWithSide::new(0o256).unwrap(),
    ///       ]
    ///   )
    /// ```
    pub fn a_side_hop1zuo1(self) -> impl Iterator<Item = PieceWithSide> {
        ASideHop1Zuo1Iter { h: self.0, i: 0 }
    }

    /// # Example
    /// ```
    /// use cetkaik_compact_representation::*;
    /// use cetkaik_core::{Color, Profession};
    /// let h = unsafe {
    ///       std::mem::transmute::<[u8; 12], Hop1zuo1>([
    ///           0b00001000, /* 兵 */ 0b00000010, /* 兵 */
    ///           0b00000000, /* 兵 */ 0b00000001, /* 兵 */
    ///           0b00000001, /* 弓 */ 0b00000000, /* 車 */
    ///           0b00000101, /* 虎 */ 0b00100000, /* 馬 */
    ///           0b00000000, /* 筆 */ 0b00100000, /* 巫 */
    ///           0b00000000, /* 将 */ 0b00001001, /* 王と船 */
    ///       ])
    ///   };
    ///
    ///   assert_eq!(
    ///       h.a_side_hop1zuo1_color_and_prof().map(|cp| cp.to_string()).collect::<Vec<_>>().join(" "),
    ///       "黒兵 赤兵 赤馬 赤巫 黒船".to_string()
    ///   )
    /// ```
    pub fn a_side_hop1zuo1_color_and_prof(
        self,
    ) -> impl Iterator<Item = cetkaik_core::ColorAndProf> {
        ASideHop1Zuo1IterWithColorAndProf { h: self.0, i: 0 }
    }
}
struct BothHop1Zuo1Iter {
    i: u8,
    h: [u8; 12],
}

struct IASideHop1Zuo1Iter {
    i: u8,
    h: [u8; 12],
}

struct ASideHop1Zuo1Iter {
    i: u8,
    h: [u8; 12],
}

struct IASideHop1Zuo1IterWithColorAndProf {
    i: u8,
    h: [u8; 12],
}

struct ASideHop1Zuo1IterWithColorAndProf {
    i: u8,
    h: [u8; 12],
}

pub mod iter;

#[cfg(test)]
mod tests {
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
}

pub type PureMove = cetkaik_core::PureMove_<Coord>;
