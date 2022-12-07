use std::num::NonZeroU8;

use cetkaik_core::{absolute, serialize_color, Color, Profession};

#[derive(Copy, Clone, Debug)]

pub struct Board([SingleRow; 9]);
type SingleRow = [Option<PieceWithSide>; 9];

/// 皇は 49、IA 側なら 1 〜 48、A 側なら最上位ビットを立てる
#[derive(Copy, Clone, Debug)]
pub struct PieceWithSide(NonZeroU8);

pub enum MaybeTam2<T> {
    Tam2,
    NotTam2(T),
}

impl PieceWithSide {
    pub fn new(u: u8) -> Option<Self> {
        if (1..=49).contains(&u) || ((128 | 1)..=(128 | 48)).contains(&u) {
            let u = std::num::NonZeroU8::new(u)?;
            Some(Self(u))
        } else {
            None
        }
    }
    /// # Safety
    /// The input must satisfy `(1..=49).contains(&u) || ((128 | 1)..=(128 | 48)).contains(&u)`
    pub const unsafe fn new_unchecked(u: u8) -> Self {
        Self(std::num::NonZeroU8::new_unchecked(u))
    }
    pub const fn color(self) -> Color {
        if self.0.get() % 2 == 1 {
            Color::Huok2
        } else {
            Color::Kok1
        }
    }
    pub const fn prof(self) -> MaybeTam2<Profession> {
        let s = self.0.get() & 127;
        if s <= 16 {
            MaybeTam2::NotTam2(Profession::Kauk2)
        } else if s <= 20 {
            MaybeTam2::NotTam2(Profession::Gua2)
        } else if s <= 24 {
            MaybeTam2::NotTam2(Profession::Kaun1)
        } else if s <= 28 {
            MaybeTam2::NotTam2(Profession::Dau2)
        } else if s <= 32 {
            MaybeTam2::NotTam2(Profession::Maun1)
        } else if s <= 36 {
            MaybeTam2::NotTam2(Profession::Kua2)
        } else if s <= 40 {
            MaybeTam2::NotTam2(Profession::Tuk2)
        } else if s <= 44 {
            MaybeTam2::NotTam2(Profession::Uai1)
        } else if s <= 46 {
            MaybeTam2::NotTam2(Profession::Io)
        } else if s <= 48 {
            MaybeTam2::NotTam2(Profession::Nuak1)
        } else {
            MaybeTam2::Tam2
        }
    }
    pub const fn prof_and_side(self) -> MaybeTam2<(Profession, absolute::Side)> {
        let s = self.0.get();
        if s <= 16 {
            MaybeTam2::NotTam2((Profession::Kauk2, absolute::Side::IASide))
        } else if s <= 20 {
            MaybeTam2::NotTam2((Profession::Gua2, absolute::Side::IASide))
        } else if s <= 24 {
            MaybeTam2::NotTam2((Profession::Kaun1, absolute::Side::IASide))
        } else if s <= 28 {
            MaybeTam2::NotTam2((Profession::Dau2, absolute::Side::IASide))
        } else if s <= 32 {
            MaybeTam2::NotTam2((Profession::Maun1, absolute::Side::IASide))
        } else if s <= 36 {
            MaybeTam2::NotTam2((Profession::Kua2, absolute::Side::IASide))
        } else if s <= 40 {
            MaybeTam2::NotTam2((Profession::Tuk2, absolute::Side::IASide))
        } else if s <= 44 {
            MaybeTam2::NotTam2((Profession::Uai1, absolute::Side::IASide))
        } else if s <= 46 {
            MaybeTam2::NotTam2((Profession::Io, absolute::Side::IASide))
        } else if s <= 48 {
            MaybeTam2::NotTam2((Profession::Nuak1, absolute::Side::IASide))
        } else if s == 49 {
            MaybeTam2::Tam2
        } else if s <= 128 | 16 {
            MaybeTam2::NotTam2((Profession::Kauk2, absolute::Side::ASide))
        } else if s <= 128 | 20 {
            MaybeTam2::NotTam2((Profession::Gua2, absolute::Side::ASide))
        } else if s <= 128 | 24 {
            MaybeTam2::NotTam2((Profession::Kaun1, absolute::Side::ASide))
        } else if s <= 128 | 28 {
            MaybeTam2::NotTam2((Profession::Dau2, absolute::Side::ASide))
        } else if s <= 128 | 32 {
            MaybeTam2::NotTam2((Profession::Maun1, absolute::Side::ASide))
        } else if s <= 128 | 36 {
            MaybeTam2::NotTam2((Profession::Kua2, absolute::Side::ASide))
        } else if s <= 128 | 40 {
            MaybeTam2::NotTam2((Profession::Tuk2, absolute::Side::ASide))
        } else if s <= 128 | 44 {
            MaybeTam2::NotTam2((Profession::Uai1, absolute::Side::ASide))
        } else if s <= 128 | 46 {
            MaybeTam2::NotTam2((Profession::Io, absolute::Side::ASide))
        } else {
            MaybeTam2::NotTam2((Profession::Nuak1, absolute::Side::ASide))
        }
    }
    pub const fn side(self) -> MaybeTam2<absolute::Side> {
        if self.0.get() == 49 {
            MaybeTam2::Tam2
        } else if self.0.get() > 127 {
            MaybeTam2::NotTam2(absolute::Side::ASide)
        } else {
            MaybeTam2::NotTam2(absolute::Side::IASide)
        }
    }
    pub const fn invert_side(self) -> Self {
        // safety: self is never 128
        unsafe { PieceWithSide::new_unchecked(self.0.get() ^ 128) }
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
use bitvec::prelude::*;

/// どちらにも属していなければ 0 を、IA 側は 1 を、A 側は 2 を
#[derive(Copy, Clone, Debug)]
pub struct Hop1zuo1(BitArr!(for 96, in u8));

#[derive(Copy, Clone, Debug)]
pub struct Field {
    board: Board,
    hop1zuo1: Hop1zuo1,
}

impl Field {
    pub fn take(&mut self, from: Coord, to: Coord) {
        assert_ne!(from, to);
        match (self.board.pop(from), self.board.pop(to)) {
            (Some(src_piece), Some(dst_piece)) => {
                self.board.put(to, Some(src_piece));
                self.hop1zuo1.set(dst_piece.invert_side(), true)
            }
            _ => panic!("Empty square encountered at either {:?} or {:?}", from, to),
        }
    }
    pub fn from_hop1zuo1(&mut self, p: PieceWithSide, to: Coord) {
        assert!(
            self.hop1zuo1.read(p),
            "cannot place {:?} ({}) because it is not in hop1zuo1",
            p,
            p
        );

        self.hop1zuo1.set(p, false);
        self.board.put(to, Some(p))
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Coord {
    row_index: usize,
    col_index: usize,
}

impl Coord {
    pub const fn new(row_index: usize, col_index: usize) -> Option<Self> {
        if row_index < 9 && col_index < 9 {
            Some(Coord {
                row_index,
                col_index,
            })
        } else {
            None
        }
    }
    pub const fn add_delta(self, row_delta: isize, col_delta: isize) -> Option<Self> {
        const fn bound(k: isize) -> Option<usize> {
            if 0 <= k && k < 9 {
                Some(k as usize)
            } else {
                None
            }
        }
        const fn add(base: usize, delta: isize) -> Option<usize> {
            bound(base as isize + delta)
        }
        //  `?` is not allowed in a `const fn`
        match (
            add(self.row_index, row_delta),
            add(self.col_index, col_delta),
        ) {
            (Some(row_index), Some(col_index)) => Some(Coord {
                row_index,
                col_index,
            }),
            _ => None,
        }
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
        write!(f, "{}{}", col, row)
    }
}

impl Board {
    pub const fn yhuap_initial() -> Self {
        Board(unsafe {
            std::mem::transmute::<[[u8; 9]; 9], [SingleRow; 9]>([
                [
                    128 | 35,
                    128 | 31,
                    128 | 23,
                    128 | 43,
                    128 | 46,
                    128 | 44,
                    128 | 24,
                    128 | 32,
                    128 | 36,
                ],
                [
                    128 | 40,
                    128 | 20,
                    0,
                    128 | 28,
                    0,
                    128 | 27,
                    0,
                    128 | 19,
                    128 | 39,
                ],
                [
                    128 | 9,
                    128 | 10,
                    128 | 11,
                    128 | 12,
                    128 | 48,
                    128 | 16,
                    128 | 15,
                    128 | 14,
                    128 | 13,
                ],
                [0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 49, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0],
                [1, 2, 3, 4, 47, 8, 7, 6, 5],
                [37, 17, 0, 25, 0, 26, 0, 18, 38],
                [34, 30, 22, 42, 45, 41, 21, 29, 33],
            ])
        })
    }
    pub fn peek(&self, c: Coord) -> Option<PieceWithSide> {
        self.0[c.row_index][c.col_index]
    }
    pub fn pop(&mut self, c: Coord) -> Option<PieceWithSide> {
        let p = self.0[c.row_index][c.col_index];
        self.put(c, None);
        p
    }
    pub fn put(&mut self, c: Coord, p: Option<PieceWithSide>) {
        self.0[c.row_index][c.col_index] = p
    }

    pub fn assert_empty(&self, c: Coord) {
        if self.peek(c).is_some() {
            panic!(
                "Expected the square {:?} to be empty, but it was occupied",
                c
            )
        }
    }

    pub fn assert_occupied(&self, c: Coord) {
        if self.peek(c).is_none() {
            panic!(
                "Expected the square {:?} to be occupied, but it was empty",
                c
            )
        }
    }

    pub fn mov(&mut self, from: Coord, to: Coord) {
        match self.pop(from) {
            None => panic!("Empty square encountered at {:?}", from),
            Some(src_piece) => {
                self.assert_empty(to);
                self.put(to, Some(src_piece))
            }
        }
    }
}

impl Default for Hop1zuo1 {
    fn default() -> Self {
        Self::new()
    }
}

impl Hop1zuo1 {
    pub fn new() -> Self {
        Hop1zuo1(BitArray::ZERO)
    }
    pub fn set(&mut self, p: PieceWithSide, flag: bool) {
        let side = p.side();
        let index = ((p.0.get() & 127) - 1) as usize;
        match side {
            MaybeTam2::NotTam2(cetkaik_core::absolute::Side::IASide) => {
                self.0.set(index * 2 + 1, flag)
            }
            MaybeTam2::NotTam2(cetkaik_core::absolute::Side::ASide) => self.0.set(index * 2, flag),
            _ => {
                panic!("Tried to put Tam2 into hop1zuo1")
            }
        }
    }
    pub fn read(&mut self, p: PieceWithSide) -> bool {
        let side = p.side();
        let index = ((p.0.get() & 127) - 1) as usize;
        match side {
            MaybeTam2::NotTam2(cetkaik_core::absolute::Side::IASide) => {
                *self.0.get(index * 2 + 1).unwrap()
            }
            MaybeTam2::NotTam2(cetkaik_core::absolute::Side::ASide) => {
                *self.0.get(index * 2).unwrap()
            }
            _ => {
                panic!("Tried to erase Tam2 into hop1zuo1")
            }
        }
    }
}

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
        let p = PieceWithSide::new(20).unwrap();
        h.set(p, true);
        assert!(h.read(p))
    }

    #[test]
    fn it_works() {
        assert_eq!(std::mem::size_of::<Field>(), 93);
    }
}
