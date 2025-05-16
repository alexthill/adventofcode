use aoc_lib_rust::{Day, Solution};
use aoc_lib_rust::utils::vector::IVec2;

pub struct Day10;

impl Day for Day10 {

    const PART1: Solution = Solution::Str("
#    #  ######   ####   #    #  #####   #####   ######  ##### 
#    #  #       #    #  #    #  #    #  #    #       #  #    #
 #  #   #       #        #  #   #    #  #    #       #  #    #
 #  #   #       #        #  #   #    #  #    #      #   #    #
  ##    #####   #         ##    #####   #####      #    ##### 
  ##    #       #         ##    #    #  #         #     #    #
 #  #   #       #        #  #   #    #  #        #      #    #
 #  #   #       #        #  #   #    #  #       #       #    #
#    #  #       #    #  #    #  #    #  #       #       #    #
#    #  ######   ####   #    #  #####   #       ######  ##### ");
    const PART2: Solution = Solution::U32(10124);

    fn solve(input: &str) -> [Solution; 2] {
        let mut points = input.lines().map(|line| {
            let mut parts = line.split(['<', '>', ',']).map(|part| part.trim_start());
            parts.next().unwrap();
            let coords = [parts.next().unwrap(), parts.next().unwrap()]
                .map(|part| part.parse::<i32>().unwrap());
            parts.next().unwrap();
            let velocity = [parts.next().unwrap(), parts.next().unwrap()]
                .map(|part| part.parse::<i32>().unwrap());
            (coords.into(), velocity.into())
        }).collect::<Vec<(IVec2, IVec2)>>();

        // use some simple heuristic to skip a lot of iterations:
        // - get some point far away from x=0
        // - skip most of the iterations it would take to move it to x=0
        let mut xmax_i = 0;
        for (i, point) in points.iter().enumerate() {
            if points[i].0[0] < point.0[0] {
                xmax_i = i;
            }
        }
        let point = points[xmax_i];
        let mut time = -point.0[0] / point.1[0] * 99 / 100;
        for point in points.iter_mut() {
            point.0 += point.1 * time;
        }

        let mut xsize = i32::MAX;
        let mut ysize = i32::MAX;
        loop {
            let mut xmax = i32::MIN;
            let mut xmin = i32::MAX;
            let mut ymax = i32::MIN;
            let mut ymin = i32::MAX;
            for point in points.iter_mut() {
                point.0 += point.1;
                xmax = xmax.max(point.0[0]);
                xmin = xmin.min(point.0[0]);
                ymax = ymax.max(point.0[1]);
                ymin = ymin.min(point.0[1]);
            }
            let new_xsize = xmax - xmin;
            let new_ysize = ymax - ymin;
            if new_xsize > xsize || new_ysize > ysize {
                break;
            }
            xsize = new_xsize;
            ysize = new_ysize;
            time += 1;
        }

        let mut xmax = i32::MIN;
        let mut xmin = i32::MAX;
        let mut ymax = i32::MIN;
        let mut ymin = i32::MAX;
        for point in points.iter_mut() {
            point.0 -= point.1;
            xmax = xmax.max(point.0[0]);
            xmin = xmin.min(point.0[0]);
            ymax = ymax.max(point.0[1]);
            ymin = ymin.min(point.0[1]);
        }
        let xsize = xmax - xmin + 2;
        let ysize = ymax - ymin + 1;

        let mut display = vec![vec![' '; xsize as usize]; ysize as usize];
        for point in points {
            let coords = point.0 - IVec2::from([xmin, ymin]);
            display[coords[1] as usize][coords[0] as usize + 1] = '#';
        }
        let sol1 = display.into_iter()
            .map(|mut line| {
                line[0] = '\n';
                line.iter().collect::<String>()
            })
            .collect::<String>();

        [Solution::String(sol1), Solution::U32(time as _)]
    }
}
