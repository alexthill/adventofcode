use crate::year::Year;

use std::fmt;
use std::io::{self, Write};
use std::process::Command;
use std::time::Duration;

const HELP: &str = r#"Usage: cargo r [command] (flags)
Commands:
    help        prints this help
    [N]         executes day N
    all         executes all days

Flags:
    -d          download the input file(s)
    -b          benchmark
    -s          sort ascending by time
"#;

#[derive(Debug, Default, Clone, Copy)]
pub struct Flags {
    pub day: u8,
    pub download: bool,
    pub benchmark: bool,
    pub sort: bool,
}

#[derive(Debug, Clone, Copy)]
pub struct BenchStat {
    day: u8,
    count: u32,
    total: Duration,
    max: Duration,
    min: Duration,
}
impl BenchStat {
    fn new(day: u8) -> Self {
        Self {
            day,
            count: 0,
            total: Duration::ZERO,
            max: Duration::ZERO,
            min: Duration::MAX,
        }
    }
    fn update(&mut self, time: Duration) {
        self.count += 1;
        self.total += time;
        self.min = if self.min < time { self.min } else { time };
        self.max = if self.max > time { self.max } else { time };
    }
    fn avg(&self) -> Duration {
        self.total.checked_div(self.count).unwrap_or_default()
    }
}
impl fmt::Display for BenchStat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f, "avg {:12?} min {:12?} max {:12?} ({:3} runs)",
            self.avg(), self.min, self.max, self.count,
        )
    }
}


pub fn cli_print<Y, A>(args: A)
    -> Result<i32, Box<dyn std::error::Error>>
where
    Y: Year,
    A: IntoIterator<Item = String>,
{
    cli::<Y, A, _>(args, &mut |s| println!("{s}"))
}

pub fn cli<Y, A, O>(args: A, out: &mut O)
    -> Result<i32, Box<dyn std::error::Error>>
where
    Y: Year,
    A: IntoIterator<Item = String>,
    O: FnMut(String),
{
    let mut args = args.into_iter().peekable();
    let _name = args.next();
    let Some(cmd) = args.next() else {
        out(HELP.to_owned());
        return Ok(1);
    };

    let mut flags = Flags::default();
    while let Some(arg) = args.peek() {
        if !arg.starts_with('-') {
            break;
        }
        match arg.as_str() {
            "-d" => flags.download = true,
            "-b" => flags.benchmark = true,
            "-s" => flags.sort = true,
            other => {
                out(format!("Error: unknown command {other}"));
                out(HELP.to_owned());
                return Ok(1);
            }
        }
        args.next();
    }

    let mut run_day = |flags: Flags| -> Result<BenchStat, io::Error> {
        let max_runs = if flags.benchmark { 100 } else { 1 };
        if flags.benchmark || flags.sort {
            print!("\rDay{:02}: ", flags.day);
            io::stdout().flush()?;
        }
        let mut stat = BenchStat::new(flags.day);
        let mut i = 0;
        while stat.total < Duration::from_secs(5) && i < max_runs {
            i += 1;
            let Some(res) = Y::solve_specific_day(flags, out)? else { break };
            stat.update(res.time);
            if flags.benchmark || flags.sort {
                print!("\rDay{:02}: {:>12?}", flags.day, stat.avg());
                io::stdout().flush()?;
            }
        }
        if flags.benchmark && !flags.sort{
            println!("\rDay{:02}: {stat}", flags.day);
        }
        Ok(stat)
    };

    let status = match cmd.as_str() {
        "help" => {
            out(HELP.to_owned());
            0
        }
        "all" => {
            if flags.download {
                let Some(cookie) = args.next() else {
                    out("please give your session cookie as argument".to_owned());
                    return Ok(1);
                };
                for day in 1..=25 {
                    download(day, Y::YEAR, &cookie, Y::INPUT_DIR)?;
                }
            } else {
                let mut results = Vec::new();
                let mut total_time = Duration::default();
                for day in 1..=25 {
                    flags.day = day;
                    let Ok(res) = run_day(flags) else {
                        eprintln!("\rWarning: running day {day} failed");
                        break;
                    };
                    results.push(res);
                    total_time += res.avg();
                }
                if flags.sort && !results.is_empty() {
                    print!("\r");
                    results.sort_unstable_by_key(|res| res.avg());
                    let w = termsize::get().map(|size| size.cols).unwrap_or(80);
                    let w = w.saturating_sub("Day00:  ".len() as u16 + 12).max(10);
                    let unit = (results.last().unwrap().avg() / w as u32).as_secs_f32();
                    for res in results {
                        let avg_secs = res.avg().as_secs_f32();
                        if avg_secs >= unit {
                            let bar = (avg_secs / unit) as usize;
                            println!(
                                "Day{:02}: {:>12?} {:█<width$}",
                                res.day, res.avg(), "", width = bar,
                            );
                        } else if avg_secs * w as f32 >= unit {
                            let bar = (avg_secs * w as f32 / unit) as usize;
                            println!(
                                "Day{:02}: {:>12?} {:▒<width$}",
                                res.day, res.avg(), "", width = bar,
                            );
                        } else {
                            let bar = (avg_secs * (w as f32).powi(2) / unit) as usize;
                            println!(
                                "Day{:02}: {:>12?} {:░<width$}",
                                res.day, res.avg(), "", width = bar,
                            );
                        }
                    }
                }
                out(format!("Total time: {total_time:?}"));
            }
            0
        }
        day if day.parse::<u8>().map(|day| (1..=25).contains(&day)).unwrap_or(false) => {
            flags.day = day.parse().unwrap();
            if flags.download {
                let Some(cookie) = args.next() else {
                    out("please give your session cookie as argument".to_owned());
                    return Ok(1);
                };
                download(flags.day, Y::YEAR, &cookie, Y::INPUT_DIR)?
            } else {
                run_day(flags)?;
                0
            }
        }
        other => {
            out(format!("Error: unknown command {other}"));
            out(HELP.to_owned());
            1
        }
    };
    Ok(status)
}

fn download(day: u8, year: u16, cookie: &str, folder: &str) -> Result<i32, Box<dyn std::error::Error>> {
    let status = Command::new("curl")
        .arg("-A")
        .arg("just_using_curl_to_download_my_inputs_please_do_not_block_me_thanks <https://github.com/alexthill/adventofcode>")
        .arg("--cookie")
        .arg(format!("session={cookie}"))
        .arg("-o")
        .arg(format!("{}/day{day:02}.txt", folder))
        .arg(format!("https://adventofcode.com/{}/day/{day}/input", year))
        .spawn()?
        .wait()?
        .code()
        .unwrap_or(0);
    Ok(status)
}

#[cfg(test)]
mod tests {
    use crate::year::MockYear;
    use super::*;

    #[test]
    fn test_no_options() {
        let mut out = Vec::new();
        let args = ["test"].map(String::from);
        let exit_status = cli::<MockYear, _, _>(args, |s| out.push(s)).unwrap();
        assert_eq!(exit_status, 1);
        assert_eq!(out, [HELP]);
    }

    #[test]
    fn test_unknown_command() {
        let mut out = Vec::new();
        let args = ["test", "foo"].map(String::from);
        let exit_status = cli::<MockYear, _, _>(args, |s| out.push(s)).unwrap();
        assert_eq!(exit_status, 1);
        assert_eq!(out, [format!("Error: unknown command foo"), HELP.to_owned()]);
    }
}
