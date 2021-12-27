use std::collections::HashMap;

struct DeterministicDice {
    last: u32,
    max: u32,
    count: u32,
}
impl DeterministicDice {
    fn new(max: u32) -> Self {
        Self { last: 0, max, count: 0 }
    }
    
    fn roll(&mut self) -> u32 {
        let val = self.last + 1;
        self.last = val % self.max;
        self.count += 1;
        val
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
struct Game {
    scores: [u32; 2],
    fields: [u32; 2],
    player: usize,
}
impl Game {
    fn new(p1_start: u32, p2_start: u32) -> Self {
        Game { scores: [0, 0], fields: [p1_start - 1, p2_start - 1], player: 0 }
    }
}

pub fn solve() {
    const P1_START: u32 = 10;
    const P2_START: u32 = 6;
    let mut dice = DeterministicDice::new(100);
    let mut scores = [0, 0];
    let mut fields = [P1_START - 1, P2_START - 1];
    let mut player = 0;
    
    loop {
        let val = dice.roll() + dice.roll() + dice.roll();
        let next_player = (player + 1) % 2;
        fields[player] = (fields[player] + val) % 10;
        scores[player] += fields[player] + 1;
        if scores[player] >= 1000 {
            println!("player {} wins, {} * {} = {}", player + 1, scores[next_player], dice.count, scores[next_player] * dice.count);
            break;
        }
        player = next_player;
    } 
    
    let mut games: HashMap<Game, usize> = HashMap::new();
    let mut win_counts: [usize; 2] = [0, 0];
    games.insert(Game::new(P1_START, P2_START), 1);
    while !games.is_empty() {
        let mut new_games = HashMap::new();
        for (game, count) in games.drain() {
            let values = [(3, 1), (4, 3), (5, 6), (6, 7), (7, 6), (8, 3), (9, 1)];
            for (val, val_count) in values.into_iter() {
                let mut new_game = game.clone();
                new_game.fields[new_game.player] = (new_game.fields[new_game.player] + val) % 10;
                new_game.scores[new_game.player] += new_game.fields[new_game.player] + 1;
                
                if new_game.scores[new_game.player] >= 21 {
                    win_counts[new_game.player] += count * val_count;
                } else {
                    new_game.player = (new_game.player + 1) % 2;
                    *new_games.entry(new_game).or_insert(0) += count * val_count;
                }
            }
        }
        games = new_games;
    }
    println!("win counts: {:?}", win_counts);
}
