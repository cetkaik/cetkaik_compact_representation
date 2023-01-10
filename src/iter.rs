use crate::PieceWithSide;

impl Iterator for crate::BothHop1Zuo1Iter {
    type Item = PieceWithSide;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        let index = self.i >> 2;
        let bit_position = 6 - (self.i & 0o03) * 2;
        let byte = *self.h.get(index as usize)?;

        if 0 != (byte & (1 << bit_position)) {
            let item = PieceWithSide::new(self.i | 0o100);
            self.i += 1;
            return item;
        }

        if 0 != (byte & (2 << bit_position)) {
            let item = PieceWithSide::new(self.i | 0o200);
            self.i += 1;
            return item;
        }

        self.i += 1;
        self.next()
    }
}

impl Iterator for crate::IASideHop1Zuo1Iter {
    type Item = PieceWithSide;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        let index = self.i >> 2;
        let bit_position = 6 - (self.i & 0o03) * 2;
        let byte = *self.h.get(index as usize)?;

        if 0 != (byte & (1 << bit_position)) {
            let item = PieceWithSide::new(self.i | 0o100);
            self.i += 1;
            return item;
        }

        self.i += 1;
        self.next()
    }
}

impl Iterator for crate::SingleSideHop1Zuo1IterWithColorAndProf {
    type Item = cetkaik_fundamental::ColorAndProf;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        use crate::{AbsoluteSide::ASide, Color, Profession};

        let index = self.i >> 2;
        let bit_position = 6 - (self.i & 0o03) * 2;
        let byte = *self.h.get(index as usize)?;

        if 0 != (byte & ((if self.side == ASide { 2 } else { 1 }) << bit_position)) {
            let color = if self.i % 2 == 0 {
                Color::Huok2
            } else {
                Color::Kok1
            };
            let prof = if self.i < 16 {
                Profession::Kauk2
            } else if self.i < 20 {
                Profession::Gua2
            } else if self.i < 24 {
                Profession::Kaun1
            } else if self.i < 28 {
                Profession::Dau2
            } else if self.i < 32 {
                Profession::Maun1
            } else if self.i < 36 {
                Profession::Kua2
            } else if self.i < 40 {
                Profession::Tuk2
            } else if self.i < 44 {
                Profession::Uai1
            } else if self.i < 46 {
                Profession::Io
            } else {
                Profession::Nuak1
            };
            self.i += 1;
            return Some(cetkaik_fundamental::ColorAndProf { color, prof });
        }
        self.i += 1;
        self.next()
    }
}

impl Iterator for crate::ASideHop1Zuo1Iter {
    type Item = PieceWithSide;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        let index = self.i >> 2;
        let bit_position = 6 - (self.i & 0o03) * 2;
        let byte = *self.h.get(index as usize)?;

        if 0 != (byte & (2 << bit_position)) {
            let item = PieceWithSide::new(self.i | 0o200);
            self.i += 1;
            return item;
        }

        self.i += 1;

        self.next()
    }
}

impl Iterator for crate::EmptySquaresIter {
    type Item = crate::Coord;

    fn next(&mut self) -> Option<Self::Item> {
        use cetkaik_traits::IsBoard;
        let coord: crate::Coord = crate::Coord::new(self.row_index, self.col_index).unwrap();
        if self.board.peek(coord).is_none() {
            return Some(coord);
        }

        if self.col_index < 9 {
            self.col_index += 1;
            return self.next();
        } else if self.row_index < 9 {
            self.col_index = 0;
            self.row_index += 1;
            return self.next();
        }

        return None;
    }
}
