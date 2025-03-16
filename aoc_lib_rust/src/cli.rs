use crate::year::Year;
use std::process::Command;

const HELP: &str = r#"Usage: cargo r [command] (flags)
Commands:
    help        prints this help
    [N]         executes day N
    all         executes all days

Flags:
    -d          download the input file(s)
"#;

#[derive(Debug, Default, Clone, Copy)]
struct Flags {
    download: bool,
}

pub fn cli_print<Y, A>(args: A)
    -> Result<i32, Box<dyn std::error::Error>>
where
    Y: Year,
    A: IntoIterator<Item = String>,
{
    cli::<Y, A, _>(args, |s| println!("{s}"))
}

pub fn cli<Y, A, O>(args: A, mut out: O)
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
        if arg.chars().next() != Some('-') {
            break;
        }
        match arg.as_str() {
            "-d" => flags.download = true,
            other => {
                out(format!("Error: unknown command {other}"));
                out(HELP.to_owned());
                return Ok(1);
            }
        }
        args.next();
    }

    let status = match cmd.as_str() {
        "help" => {
            out(HELP.to_owned());
            0
        }
        "all" => {
            if flags.download {
                let Some(cookie) = args.next() else {
                    out(format!("please give your session cookie as argument"));
                    return Ok(1);
                };
                for day in 1..=25 {
                    download(day, Y::YEAR, &cookie, Y::INPUT_DIR)?;
                }
            } else {
                for day in 1..=25 {
                    Y::solve_specific_day(day, &mut out)?;
                }
            }
            0
        }
        day if day.parse::<u8>().map(|day| (1..=25).contains(&day)).unwrap_or(false) => {
            let day = day.parse().unwrap();
            if flags.download {
                let Some(cookie) = args.next() else {
                    out(format!("please give your session cookie as argument"));
                    return Ok(1);
                };
                download(day, Y::YEAR, &cookie, Y::INPUT_DIR)?
            } else {
                Y::solve_specific_day(day, &mut out)?;
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
        .arg(&format!("session={cookie}"))
        .arg("-o")
        .arg(&format!("{}/day{day:02}.txt", folder))
        .arg(&format!("https://adventofcode.com/{}/day/{day}/input", year))
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
