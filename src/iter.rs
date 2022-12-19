use crate::PieceWithSide;

impl Iterator for crate::BothHop1Zuo1 {
    type Item = PieceWithSide;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        let index = self.i >> 2;
        let bit_position = 6 - (self.i & 0o03) * 2;
        unsafe {
            let byte = *self.h.get_unchecked(index as usize);

            if 0 != (byte & (1 << bit_position)) {
                let item = Some(PieceWithSide::new_unchecked(self.i | 0o100));
                self.i += 1;
                return item;
            }

            if 0 != (byte & (2 << bit_position)) {
                let item = Some(PieceWithSide::new_unchecked(self.i | 0o200));
                self.i += 1;
                return item;
            }
        }
        self.i += 1;

        if self.i >= 0o60 {
            return None;
        }
        self.next()
    }
}

impl Iterator for crate::IASideHop1Zuo1 {
    type Item = PieceWithSide;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        let index = self.i >> 2;
        let bit_position = 6 - (self.i & 0o03) * 2;
        unsafe {
            let byte = *self.h.get_unchecked(index as usize);

            if 0 != (byte & (1 << bit_position)) {
                let item = Some(PieceWithSide::new_unchecked(self.i | 0o100));
                self.i += 1;
                return item;
            }
        }
        self.i += 1;

        if self.i >= 0o60 {
            return None;
        }
        self.next()
    }
}

impl Iterator for crate::ASideHop1Zuo1 {
    type Item = PieceWithSide;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        let index = self.i >> 2;
        let bit_position = 6 - (self.i & 0o03) * 2;
        unsafe {
            let byte = *self.h.get_unchecked(index as usize);

            if 0 != (byte & (2 << bit_position)) {
                let item = Some(PieceWithSide::new_unchecked(self.i | 0o200));
                self.i += 1;
                return item;
            }
        }
        self.i += 1;

        if self.i >= 0o60 {
            return None;
        }
        self.next()
    }
}
