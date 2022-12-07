use std::num::NonZeroU8;

use cetkaik_core::{absolute, Color, Profession};

#[derive(Copy, Clone)]

pub struct Board([SingleRow; 9]);
pub type SingleRow = [Option<PieceWithSide>; 9];

/// 1 〜 49
#[derive(Copy, Clone)]
pub struct Piece(NonZeroU8);

impl Piece {
    pub const fn color(self) -> Color {
        if self.0.get() % 2 == 1 {
            Color::Kok1
        } else {
            Color::Huok2
        }
    }
    pub const fn prof(self) -> MaybeTam2<Profession> {
        let s = self.0.get();
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
}

/// IA 側なら 1 〜 49、A 側なら最上位ビットを立てる
#[derive(Copy, Clone)]
pub struct PieceWithSide(NonZeroU8);

pub enum MaybeTam2<T> {
    Tam2,
    NotTam2(T),
}

impl PieceWithSide {
    pub const fn color(self) -> Color {
        if self.0.get() % 2 == 1 {
            Color::Kok1
        } else {
            Color::Huok2
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
        unsafe { PieceWithSide(std::num::NonZeroU8::new_unchecked(self.0.get() ^ 128)) }
    }
}

use bitvec::prelude::*;

/// どちらにも属していなければ 0 を、IA 側は 1 を、A 側は 2 を
#[derive(Copy, Clone)]
pub struct Hop1zuo1(BitArr!(for 96));

#[derive(Copy, Clone)]
pub struct Field {
    board: Board,
    hop1zuo1: Hop1zuo1,
}

pub struct Coord {
    row_index: usize,
    col_index: usize,
}

impl Board {
    pub fn get(&self, c: Coord) -> Option<PieceWithSide> {
        self.0[c.row_index][c.col_index]
    }
    pub fn set(&mut self, c: Coord, p: Option<PieceWithSide>) {
        self.0[c.row_index][c.col_index] = p
    }
}

impl Hop1zuo1 {
    pub fn put(&mut self, p: PieceWithSide) {
        let side = p.side();
        let index = ((p.0.get() & 127) - 1) as usize;
        match side {
            MaybeTam2::NotTam2(cetkaik_core::absolute::Side::IASide) => {
                self.0.set(index * 2 + 1, true)
            }
            MaybeTam2::NotTam2(cetkaik_core::absolute::Side::ASide) => self.0.set(index * 2, true),
            _ => {
                panic!("Tried to put Tam2 into hop1zuo1")
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(std::mem::size_of::<Field>(), 93);
    }
}
