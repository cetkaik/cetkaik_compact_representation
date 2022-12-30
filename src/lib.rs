#![warn(clippy::pedantic, clippy::nursery)]
#![allow(clippy::unreadable_literal)]
use std::num::NonZeroU8;

use cetkaik_fundamental::{serialize_color, AbsoluteSide, Color, Profession};

use cetkaik_traits::{
    CetkaikRepresentation, IsAbsoluteBoard, IsAbsoluteField, IsBoard, IsField, IsPieceWithSide,
};

#[derive(Copy, Clone, Debug)]
#[repr(C)]
pub struct Board([SingleRow; 9]);

impl Board {
    #[must_use]
    pub const fn to_piece_array(self) -> [[Option<PieceWithSide>; 9]; 9] {
        self.0
    }

    #[must_use]
    pub const fn to_u8_array(self) -> [[u8; 9]; 9] {
        unsafe { std::mem::transmute::<Self, [[u8; 9]; 9]>(self) }
    }
    /// # Example
    /// ```
    /// use cetkaik_compact_representation::*;
    /// use cetkaik_traits::IsAbsoluteBoard;
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
    /// use cetkaik_traits::IsAbsoluteBoard;
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
    /// use cetkaik_traits::IsAbsoluteBoard;
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
#[repr(C)]
pub struct PieceWithSide(NonZeroU8);

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
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
    pub const fn prof_and_side(self) -> MaybeTam2<(Profession, AbsoluteSide)> {
        if self.is_tam2() {
            return MaybeTam2::Tam2;
        }
        let s = if self.0.get() & 0o300 == 0o100 {
            AbsoluteSide::IASide
        } else {
            AbsoluteSide::ASide
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
    /// assert!(a.can_be_moved_by(cetkaik_fundamental::AbsoluteSide::ASide));
    /// assert!(!b.can_be_moved_by(cetkaik_fundamental::AbsoluteSide::ASide));
    /// assert!(tam.can_be_moved_by(cetkaik_fundamental::AbsoluteSide::ASide));
    /// assert!(b.can_be_moved_by(cetkaik_fundamental::AbsoluteSide::IASide));
    /// assert!(!a.can_be_moved_by(cetkaik_fundamental::AbsoluteSide::IASide));
    /// assert!(tam.can_be_moved_by(cetkaik_fundamental::AbsoluteSide::IASide));
    /// ```
    #[must_use]
    pub const fn can_be_moved_by(self, side: cetkaik_fundamental::AbsoluteSide) -> bool {
        let u8 = self.0.get();
        match side {
            AbsoluteSide::ASide => 0o200 & u8 != 0,
            AbsoluteSide::IASide => 0o100 & u8 != 0,
        }
    }
}

impl std::fmt::Display for PieceWithSide {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use cetkaik_fundamental::serialize_prof;
        match self.prof_and_side() {
            MaybeTam2::Tam2 => write!(f, "皇"),
            MaybeTam2::NotTam2((prof, side)) => write!(
                f,
                "{}{}{}",
                if side == AbsoluteSide::IASide {
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

impl IsPieceWithSide for PieceWithSide {
    type Side = AbsoluteSide;

    fn match_on_piece_and_apply<U>(
        self,
        f_tam: &dyn Fn() -> U,
        f_piece: &dyn Fn(Color, Profession, Self::Side) -> U,
    ) -> U {
        match self.prof_and_side() {
            crate::MaybeTam2::Tam2 => f_tam(),
            crate::MaybeTam2::NotTam2((prof, side)) => f_piece(self.color(), prof, side),
        }
    }
}

/// どちらにも属していなければ 0 を、IA 側は 1 を、A 側は 2 を
#[derive(Copy, Clone, Debug)]
#[repr(C)]
pub struct Hop1zuo1([u8; 12]);

#[derive(Copy, Clone, Debug)]
#[repr(C)]
pub struct Field {
    board: Board,
    hop1zuo1: Hop1zuo1,
}

impl IsField for Field {
    type Board = Board;
    type Coord = Coord;
    type PieceWithSide = PieceWithSide;
    type Side = cetkaik_fundamental::AbsoluteSide;

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
            return Err("not the right owner");
        }

        // self-occlusion
        if from == to {
            return Ok(*self);
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

    fn as_board(&self) -> &Board {
        &self.board
    }

    fn as_board_mut(&mut self) -> &mut Board {
        &mut self.board
    }

    fn search_from_hop1zuo1_and_parachute_at(
        &self,
        color: Color,
        prof: Profession,
        side: cetkaik_fundamental::AbsoluteSide,
        dest: Coord,
    ) -> Option<Self> {
        for piece in self.as_hop1zuo1().both_hop1zuo1() {
            if piece.color() == color && piece.prof_and_side() == MaybeTam2::NotTam2((prof, side)) {
                let mut new_self = *self;
                new_self.as_hop1zuo1_mut().clear(piece);
                new_self.as_board().assert_empty(dest);
                new_self.as_board_mut().put(dest, Some(piece));
                return Some(new_self);
            }
        }
        None
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
    /// use cetkaik_fundamental::{Color, Profession};
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
    ) -> impl Iterator<Item = cetkaik_fundamental::ColorAndProf> {
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
    /// use cetkaik_fundamental::{Color, Profession};
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
    ) -> impl Iterator<Item = cetkaik_fundamental::ColorAndProf> {
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

#[warn(clippy::pedantic, clippy::nursery)]
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

pub type PureMove = cetkaik_fundamental::PureMove_<Coord>;

#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub struct CetkaikCompact;

/// `cetkaik_compact_representation` クレートに基づいており、視点を決め打ちして絶対座標=相対座標として表現する。
/// この impl においては、IAは常に一番下の行であり、初期状態でIA行を占有していたプレイヤーは駒が上向き（=あなた）である。
/// つまり、`Upward` は常に `IASide` へと読み替えられる。
impl CetkaikRepresentation for CetkaikCompact {
    type Perspective = crate::Perspective;

    type AbsoluteCoord = crate::Coord;
    type RelativeCoord = crate::Coord;

    type AbsoluteBoard = crate::Board;
    type RelativeBoard = crate::Board;

    type AbsolutePiece = crate::PieceWithSide;
    type RelativePiece = crate::PieceWithSide;

    type AbsoluteField = crate::Field;
    type RelativeField = crate::Field;

    type RelativeSide = AbsoluteSide; // ここも absolute
    fn to_absolute_coord(coord: Self::RelativeCoord, _p: Self::Perspective) -> Self::AbsoluteCoord {
        coord
    }
    fn to_relative_coord(coord: Self::AbsoluteCoord, _p: Self::Perspective) -> Self::RelativeCoord {
        coord
    }
    fn add_delta(
        coord: Self::RelativeCoord,
        row_delta: isize,
        col_delta: isize,
    ) -> Option<Self::RelativeCoord> {
        crate::Coord::add_delta(coord, row_delta, col_delta)
    }
    fn relative_get(
        board: Self::RelativeBoard,
        coord: Self::RelativeCoord,
    ) -> Option<Self::RelativePiece> {
        board.peek(coord)
    }
    fn relative_clone_and_set(
        board: &Self::RelativeBoard,
        coord: Self::RelativeCoord,
        p: Option<Self::RelativePiece>,
    ) -> Self::RelativeBoard {
        let mut new_board = *board;
        new_board.put(coord, p);
        new_board
    }
    fn absolute_get(
        board: &Self::AbsoluteBoard,
        coord: Self::AbsoluteCoord,
    ) -> Option<Self::AbsolutePiece> {
        board.peek(coord)
    }
    fn is_tam_hue_by_default(coord: Self::RelativeCoord) -> bool {
        Self::RelativeCoord::is_tam_hue_by_default(coord)
    }
    fn relative_tam2() -> Self::RelativePiece {
        unsafe { crate::PieceWithSide::new_unchecked(0o300) }
    }
    fn absolute_tam2() -> Self::AbsolutePiece {
        unsafe { crate::PieceWithSide::new_unchecked(0o300) }
    }
    fn is_upward(s: Self::RelativeSide) -> bool {
        s == AbsoluteSide::IASide
    }
    fn empty_squares_relative(board: &Self::RelativeBoard) -> Vec<Self::RelativeCoord> {
        let mut ans = vec![];
        for rand_i in 0..9 {
            for rand_j in 0..9 {
                let coord: Self::RelativeCoord = Self::RelativeCoord::new(rand_i, rand_j).unwrap();
                if board.peek(coord).is_none() {
                    ans.push(coord);
                }
            }
        }
        ans
    }
    fn empty_squares_absolute(board: &Self::RelativeBoard) -> Vec<Self::RelativeCoord> {
        Self::empty_squares_relative(board)
    }

    /// ```
    /// use cetkaik_compact_representation::*;
    /// use cetkaik_fundamental::*;
    /// use cetkaik_traits::CetkaikRepresentation;
    /// let field = unsafe { std::mem::transmute::<[u8; 93], Field>([
    ///     162, 158, 150, 0, 173, 0, 151, 159, 163, 
    ///     167, 147, 170, 155, 0, 154, 171, 146, 166, 
    ///     136, 137, 0, 139, 110, 143, 142, 141, 140, 
    ///     0, 80, 0, 0, 0, 0, 0, 0, 0, 
    ///     0, 0, 0, 0, 0, 0, 0, 0, 0,
    ///     0, 0, 0, 0, 0, 0, 192, 0, 0, 
    ///     64, 65, 66, 67, 0, 71, 70, 69, 68, 
    ///     100, 0, 0, 88, 0, 89, 0, 81, 101, 
    ///     97, 93, 85, 105, 108, 104, 84, 92, 96, 
    ///     0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 1
    /// ]) };
    /// 
    /// assert_eq!(
    ///     <CetkaikCompact as CetkaikRepresentation>::hop1zuo1_of(AbsoluteSide::IASide, &field).iter()
    ///                     .map(|c| c.to_string())
    ///                     .collect::<Vec<_>>()
    ///                     .join(" "),
    ///     "黒兵 赤船"
    /// );
    /// assert_eq!(
    ///     <CetkaikCompact as CetkaikRepresentation>::hop1zuo1_of(AbsoluteSide::ASide, &field).iter()
    ///                     .map(|c| c.to_string())
    ///                     .collect::<Vec<_>>()
    ///                     .join(" "),
    ///     ""
    /// );
    /// ```
    fn hop1zuo1_of(
        side: AbsoluteSide,
        field: &Self::AbsoluteField,
    ) -> Vec<cetkaik_fundamental::ColorAndProf> {
        match side {
            AbsoluteSide::ASide => field
                .to_hop1zuo1()
                .a_side_hop1zuo1_color_and_prof()
                .collect(),
            AbsoluteSide::IASide => field
                .to_hop1zuo1()
                .ia_side_hop1zuo1_color_and_prof()
                .collect(),
        }
    }
    fn as_board_absolute(field: &Self::AbsoluteField) -> &Self::AbsoluteBoard {
        field.as_board()
    }
    fn as_board_mut_absolute(field: &mut Self::AbsoluteField) -> &mut Self::AbsoluteBoard {
        field.as_board_mut()
    }
    fn as_board_relative(field: &Self::RelativeField) -> &Self::RelativeBoard {
        field.as_board()
    }
    fn is_water_relative(c: Self::RelativeCoord) -> bool {
        crate::Coord::is_water(c)
    }
    fn is_water_absolute(c: Self::AbsoluteCoord) -> bool {
        crate::Coord::is_water(c)
    }
    fn loop_over_one_side_and_tam(
        board: &Self::RelativeBoard,
        side: Self::RelativeSide,
        f_tam_or_piece: &mut dyn FnMut(Self::RelativeCoord, Option<Profession>),
    ) {
        let fun = |(src, piece): (Self::RelativeCoord, Self::RelativePiece)| match piece
            .prof_and_side()
        {
            crate::MaybeTam2::Tam2 => f_tam_or_piece(src, None),
            crate::MaybeTam2::NotTam2((prof, _)) => {
                f_tam_or_piece(src, Some(prof));
            }
        };
        match side {
            AbsoluteSide::ASide => board.a_side_and_tam().for_each(fun),
            AbsoluteSide::IASide => board.ia_side_and_tam().for_each(fun),
        }
    }

    fn to_relative_field(field: Self::AbsoluteField, _p: Self::Perspective) -> Self::RelativeField {
        field
    }

    fn to_relative_side(side: AbsoluteSide, _p: Self::Perspective) -> Self::RelativeSide {
        side
    }
    fn get_one_perspective() -> Self::Perspective {
        // the sole choice available
        crate::Perspective::IaIsDownAndPointsUpward
    }

    fn absolute_distance(a: Self::AbsoluteCoord, b: Self::AbsoluteCoord) -> i32 {
        crate::Coord::distance(a, b)
    }

    fn absolute_same_direction(
        origin: Self::AbsoluteCoord,
        a: Self::AbsoluteCoord,
        b: Self::AbsoluteCoord,
    ) -> bool {
        crate::Coord::same_direction(origin, a, b)
    }

    fn has_prof_absolute(piece: Self::AbsolutePiece, prof: Profession) -> bool {
        match piece.prof() {
            MaybeTam2::Tam2 => false,
            MaybeTam2::NotTam2(self_prof) => prof == self_prof,
        }
    }
}

impl IsAbsoluteField for Field {
    /// The initial arrangement of the official (yhuap) rule
    fn yhuap_initial() -> Self {
        Self {
            board: Board::yhuap_initial(),
            hop1zuo1: Hop1zuo1::new(),
        }
    }
}
