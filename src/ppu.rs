use ringbuffer::ConstGenericRingBuffer;
use bitfield::bitfield;

type Memory = [u8; 0x10000];
type PixQueue = ConstGenericRingBuffer<Pixel, 8>;

const NUM_TILESET_BYTES: usize = 16384;

pub struct Ppu {
    pixel_fifo: PixQueue,
    bg_fifo: PixQueue,
    background: Box<[u8]>,
    window: Box<[u8]>,
    state: State,
}

impl Ppu {
    fn new() -> Self {
        Self {
            pixel_fifo: PixQueue::new(),
            bg_fifo: PixQueue::new(),
            background: Vec::with_capacity(NUM_TILESET_BYTES).into_boxed_slice(),
            window: Vec::with_capacity(NUM_TILESET_BYTES).into_boxed_slice(),
            state: State::OamScan,
        }
    }

    fn step(&mut self) {
        use State::*;
        match self.state {
            OamScan => {}
            HBlank => {}
            VBlank => {}
            _ => {}
        }
    }
}

struct Pixel {
    color: Color,
    palette: Palette,
    bg_priority: bool,
}

#[repr(C)]
struct ObjectAttribute {
    ypos: u8,
    xpos: u8,
    tile_num: u8,
    flags: ObjFlags,
}

bitfield! {
    pub struct ObjFlags(u8);
    impl Debug;
    u8;
    priority, set_priority: 7;
    y_flip, set_y_flip: 6;
    x_flip, set_x_flip: 5;
    palette, set_palette: 4;
}

#[repr(u8)]
enum Color {
    Id0 = 0,
    Id1 = 1,
    Id2 = 2,
    Id3 = 3,
}

#[repr(u8)]
enum Palette {
    Obp0 = 0,
    Obp1 = 1,
}

#[repr(u8)]
enum State {
    OamScan, // Mode 2
    Drawing, // Mode 3 ----
    FetchTileNo,
    FetchTileDataLo,
    FetchTileDataHi,
    PushToFifo,
    HBlank,  // Mode 0
    VBlank,  // Mode 1
}
