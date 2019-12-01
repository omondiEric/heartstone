# README

## Description
Our project emulates the game Firestone, based on the popular board game Heartstone. Firestone is a strategy game with two players. Each player has a certain mana value and a hero with a certain health value. The game is over when one of the heroes has lost all its health.

Each player has a deck and a hand of cards that all have certain capabilities. These capabilities can override the general rules of the game. In the beginning of the game each player starts with a number of cards in the hand, a number of cards in the deck and a certain mana.

The player in turn can:

attack enemy characters once with each friendly minion on the board.
choose to play a number of cards from the hand.
end the turn.
use the hero power of the hero

### Attacking
If a minion attacks another minion, the health of each minion will be reduced by the attack value of the other minion. If the remaining health of the minion is zero or below, then the minion will be destroyed and removed from the board. If a minion attacks the enemy hero, then the hero will not defend itself, hence only the hero will take damage. A minion with zero attack value can not attack. A minion can not attack the same turn it was summoned, then the minion is sleepy. A minion can only attack once per turn.

### Playing cards
Each card costs a certain mana to play and can be played if the player has enough mana. After the card is played the mana of the card is reduced from the mana of the player.

The maximum number of minions that each player can have on the board is 7.

### Ending turn
The end turn procedure is as follows:

The mana of the new player in turn is refreshed and increased by one up to a total of 10. The game then continues with 10 available mana each turn until the game is over.
The new player in turn draws a card from the deck.
Using the hero power
You can use the hero power once per turn.

When cards are drawn from the deck
If the player has 10 cards in hand the card is burned, otherwise it is added to the hand.

If a deck runs out of cards and a player is suppost to draw a card, that players hero takes damage. The damage is 1 the first time this happens. The damage increases with 1 every time this phenomena occurs.

## Implementation
This project is written in Clojure and can be run on the browser. You can run it locally by starting the server and visiting https://conjoin-it.se/firestone-ajax/?theme=hearthstone on a browser. 


## Planning document outlining our plans and goals achieved for each sprint: 
https://docs.google.com/document/d/1Z0TQg6t0_hzm04uQa3Sxht9ubH526gfCtT366ddC99U/edit?usp=sharing





 


 

