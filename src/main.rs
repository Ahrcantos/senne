mod bus;
mod processor;

use crossterm::event::{self, Event, KeyCode, KeyEvent};
use ratatui::{
    layout::{Constraint, Layout},
    style::{Color, Stylize},
    text::{Line, Text},
    widgets::{Block, Paragraph},
};

use crate::processor::{ExecutionState, Processor};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut terminal = ratatui::init();
    let mut processor = Processor::new();
    loop {
        terminal.draw(|frame| {
            let top_layout =
                Layout::horizontal([Constraint::Percentage(80), Constraint::Percentage(20)])
                    .split(frame.area());

            let right_layout = Layout::vertical([
                Constraint::Percentage(40),
                Constraint::Percentage(30),
                Constraint::Percentage(30),
            ])
            .split(top_layout[1]);

            let reg_block = {
                let block = Block::bordered().title("Registers");

                let acc_line = {
                    let acc = format!("{:02X}", processor.accumulator());

                    let mut line = Line::default();
                    line.push_span("ACC".red());
                    line.push_span(": ");
                    line.push_span(acc.bg(Color::Black).fg(Color::White));

                    line
                };

                let index_x_line = {
                    let acc = format!("{:02X}", processor.index_x());

                    let mut line = Line::default();
                    line.push_span("X  ".magenta());
                    line.push_span(": ");
                    line.push_span(acc.bg(Color::Black).fg(Color::White));

                    line
                };

                let index_y_line = {
                    let acc = format!("{:02X}", processor.index_y());

                    let mut line = Line::default();
                    line.push_span("Y  ".green());
                    line.push_span(": ");
                    line.push_span(acc.bg(Color::Black).fg(Color::White));

                    line
                };

                let pc_line = {
                    let pc = format!("{:04X}", processor.program_counter());
                    let mut line = Line::default();
                    line.push_span("PC".blue());
                    line.push_span(" : ");
                    line.push_span(pc.bg(Color::Black).fg(Color::White));

                    line
                };

                let mut text = Text::default();
                text.push_line(acc_line);
                text.push_line(index_x_line);
                text.push_line(index_y_line);
                text.push_line(pc_line);

                let paragraph = Paragraph::new(text);

                paragraph.block(block)
            };

            let flags_block = {
                let block = Block::bordered().title("Flags");
                let mut text = Text::default();

                text.push_line(format!("NEGATIVE = {}", processor.status().negative()));
                text.push_line(format!("OVERFLOW = {}", processor.status().overflow()));
                text.push_line(format!("DECIMAL = {}", processor.status().decimal()));
                text.push_line(format!(
                    "INTERRUPT_DISABLE = {}",
                    processor.status().interrupt_disable()
                ));
                text.push_line(format!("ZERO = {}", processor.status().zero()));
                text.push_line(format!("CARRY = {}", processor.status().carry()));

                let paragraph = Paragraph::new(text);
                paragraph.block(block)
            };

            let internals_block = {
                let block = Block::bordered().title("Internals");

                let mut text = Text::default();
                text.push_span("State: ");

                match processor.execution_state() {
                    ExecutionState::Ready => {
                        text.push_span("Ready".green());
                    }
                    ExecutionState::Pending { remaining_ticks } => {
                        text.push_span(format!("Pending ({})", remaining_ticks).red());
                    }
                }

                let paragraph = Paragraph::new(text);

                paragraph.block(block)
            };

            let mem_block = {
                let block = Block::bordered().title("Memory");
                let mut text = Text::default();

                let stack_top = (processor.stack_pointer() as u16 + 0x100 + 1) as usize;
                for (i, value) in processor.vram().iter().take(2048).enumerate() {
                    if i % 32 == 0 {
                        text.push_line("\n");
                    }

                    let is_on_stack = i > stack_top && i <= 0x1FF;

                    let color = if processor.program_counter() as usize == i {
                        Color::Blue
                    } else if i == stack_top && stack_top <= 0x1FF {
                        Color::Yellow
                    } else if is_on_stack {
                        Color::LightYellow
                    } else {
                        Color::White
                    };

                    text.push_span(format!("[{:02X}]", value).fg(color));
                }

                let paragraph = Paragraph::new(text);
                paragraph.block(block)
            };

            frame.render_widget(mem_block, top_layout[0]);
            frame.render_widget(reg_block, right_layout[0]);
            frame.render_widget(flags_block, right_layout[1]);
            frame.render_widget(internals_block, right_layout[2]);
        })?;

        if matches!(
            event::read().expect("failed to read event"),
            Event::Key(KeyEvent {
                code: KeyCode::Enter,
                ..
            })
        ) {
            processor.tick();
            continue;
        }

        if matches!(event::read().expect("failed to read event"), Event::Key(_)) {
            break;
        }
    }

    ratatui::restore();

    Ok(())
}
