mod cpu;
mod micro_ops;

use cpu::Cpu;
use crate::micro_ops::MicroOp;
use std::env;
use std::io;
use io::Read;
use io::BufReader;
use io::stdin;
use std::fs::File;

use sdl2::render::Canvas;
use sdl2::keyboard::Keycode;
use sdl2::VideoSubsystem;
use sdl2::pixels::Color;
use sdl2::event::Event;
use std::time::Duration;

fn main() -> io::Result<()> {
    let mut cpu = Cpu::new();
    let mut time: usize = 0;

    let args: Vec<String> = env::args().collect();

    if args.len() == 1 {
        println!("Please provide a file name to read from.");
        return Ok(());
    }

    let file_name = &args[0];

    let file = File::open(file_name.as_str())?;

    let mut reader = BufReader::new(file);
    let mut buffer = Vec::new();

    reader.read_to_end(&mut buffer)?;
    for (i, &byte) in buffer.iter().enumerate() {
        cpu.memory[i as u16] = byte;
    }
    let mut s = String::new();

    let sdl_context = sdl2::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();
    let window = video_subsystem.window("gbrs", 160, 144).position_centered().build().unwrap();
    let mut canvas = window.into_canvas().build().unwrap();
    canvas.set_draw_color(Color::RGB(255, 255, 255));
    canvas.clear();
    canvas.present();

    let mut event_pump = sdl_context.event_pump().unwrap();
    let mut i = 0;

    'running: loop {
        i = (i + 1) % 255;
        canvas.set_draw_color(Color::RGB(i, 64, 255 - i));
        canvas.clear();
        for event in event_pump.poll_iter() {
            match event {
                Event::Quit {..} |
                Event::KeyDown { keycode: Some(Keycode::Escape), .. } => {
                    break 'running
                },
                _ => {}
            }
        }

        if time % 4 == 0 {
            if cpu.instr_complete || cpu.cur_micro_ops[cpu.micro_op_index as usize] == MicroOp::End {
                println!("Next instruction");
                cpu.instr_complete = false;
                cpu.micro_op_index = 0;
                cpu.fetch();
                cpu.decode();
            }
            // stdin().read_line(&mut s).unwrap();
            cpu.step();
            println!("CPU: {:?}", cpu);
        }
        time = time.wrapping_add(1);

        canvas.present();
        ::std::thread::sleep(Duration::new(0, 1_000_000_000u32 / 60));
    }

    Ok(())
}
