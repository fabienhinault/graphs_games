"use strict";



function range(size, startAt = 0) {
    return [...Array(size).keys()].map(i => i + startAt);
}

function getRandomInt(min, max) {
  return Math.floor(Math.random() * (max - min) + min);
}

function pick(array) {
    return array[getRandomInt(0, array.length)];
}

function sum(array) {
    return array.reduce((acc, cur) => acc + cur);
}

function average(array) {
    return sum(array) / array.length;
}

function pickWeighted(weighteds) {
    const summedWeights = weighteds.reduce((acc, cur) => {
        acc.push((acc[acc.length - 1] ?? 0) + cur.weight);
        return acc;
    }, []);
    const r = Math.random() * summedWeights[summedWeights.length - 1];
    return weighteds[summedWeights.findIndex(aw => aw >= r)];
}

function min(array, f) {
    return array.reduce((acc, cur) => {
        const currentValue = f(cur);
        if (currentValue < acc.value) {
            return {elements: [cur], value: currentValue};
        } else if (currentValue === acc.value) {
            return {elements: [...acc.elements, cur], value: currentValue};
        } else {
            return acc;
        }
    },
    {value: Number.MAX_VALUE});
}

function argsMin(array, f) {
    return min(array, f).elements;
}

class Clock {
    getTime() {
        return Date.now();
    }
}

const firstPlayerValue = 1000;
const secondPlayerValue = 0;
const unsure = (firstPlayerValue + secondPlayerValue) / 2;
const valueThreshold = Math.abs((firstPlayerValue - secondPlayerValue) / 4);

class Player {
    constructor(value, bestValueFunction, attenuationFactor) {
        this.value = value;
        this.bestValueFunction = bestValueFunction;
    }
    
    getOtherPlayer() {
        return otherPlayers[this.value];
    }
    
    static getLastPlayer(moves) {
        return players[(moves.length % 2) * firstPlayerValue];
    }
    
    getProbable() {
        return (unsure + this.value) / 2;
    }

    getBestValue(nextsValues) {
        return this.bestValueFunction(...nextsValues);
    }

    minmax(nextsValues) {
        return minmaxes[this.value](...nextsValues);
    }

    // attenuate the value to be given to a sequence.
    // Typically the sequence will be given the value of the player attenuated to take
    // some circumstances in account.
    // For example a sequence with one winning next move and many losing next moves is not as winning as
    // a sequence with only winning next moves and no losing next move.
    attenuate(value, weight) {
        return value - attenuationFactor * weight; 
    }

    // wether the value of a sequence is winning for player
    isWinning(sequenceValue) {
        return Math.abs(sequenceValue - this.value) < valueThreshold;
    }
}

const attenuationFactor = 0.02 * (firstPlayerValue - unsure);
const firstPlayer = new Player(firstPlayerValue, Math.max, attenuationFactor);
const secondPlayer = new Player(secondPlayerValue, Math.min, -attenuationFactor);
let players = [];
[firstPlayer, secondPlayer].forEach(p => {players[p.value] = p;});
let otherPlayers = [firstPlayer];
otherPlayers[firstPlayer.value] = secondPlayer;
let minmaxes = [Math.min];
minmaxes[firstPlayer.value] = Math.max;
const probableFirstPlayer = firstPlayer.getProbable();
const probableSecondPlayer = secondPlayer.getProbable();
const sequenceValuesMap = new Map([
    [probableSecondPlayer, probableSecondPlayer],
    [(probableSecondPlayer + unsure) / 2 , probableSecondPlayer],
    [unsure, unsure],
    [(unsure + probableFirstPlayer) / 2, probableFirstPlayer], 
    [probableFirstPlayer, probableFirstPlayer]
]);

function otherPlayerValue(playerValue) {
    return firstPlayerValue - playerValue;
}

function checkNotNan(n) {
    if (Number.isNaN(n)) {
        throw new Error();
    }
    return n;
}

class Game {
    constructor(nextss, initialNextss, clock, gameOverCallback) {
        this.nextss = nextss;
        this.moves = [];
        this.possibleNexts = undefined;
        this.initialNextss = initialNextss;
        this.clock = clock;
        this.gameOverCallback = gameOverCallback;
        this.currentPlayer = firstPlayer;
    }
    
    copy() {
        const result = new Game([...this.nextss.map(_ => [..._])], this.initialNextss, this.clock, null);
        result.moves = [...this.moves];
        return result;
    }

    getCurrentMove() {
        return this.moves[this.moves.length - 1];
    }

    getPreviousMove() {
        return this.moves[this.moves.length - 2];
    }

    getCurrentPlayer() {
        return (this.moves.length % 2) * firstPlayer;
    }

    play(current) {
        this.moves.push(current);
        this.currentPlayer = this.currentPlayer.getOtherPlayer();
        this.possibleNexts = [...this.nextss[current]];
        this.nextss = this.nextss.map((nexts, iNexts) => {
            return nexts.filter(next => next != current);
        });
        this.nextss[current] = [];
        if (this.possibleNexts.length === 0) {
            // the game is over
            this.gameOverCallback?.(this.moves.length % 2);
        }
    }

    getLastMove() {
        return this.moves[this.moves.length - 1];
    }

    getNextMoves() {
        return this.nextss[this.getLastMove()];
    }
}


class Evaluator {
    constructor(game, gameOverCallback, sequenceValueStorage) {
        this.game = game;
        this.gameOverCallback = gameOverCallback;
        this.sequenceValueStorage= sequenceValueStorage;
    }

    /* The value of a sequence says if the sequence is winning for firstPlayer or secondPlayer.
     * It is winning for firstPlayer if the value is close to firstPlayerValue, and same for secondPlayer.
     * The value of the sequence depends on the value of its successors.
     */
    evaluateSequence(sequence) {
        const lastMove = sequence[sequence.length - 1];
        // player who just played last move
        const lastPlayer = Player.getLastPlayer(sequence);
        const nextPlayer = lastPlayer.getOtherPlayer();
        const nexts = this.game.initialNextss[lastMove].filter(_ => !sequence.includes(_));
        const nextsValues = nexts.map(next => this.getSequenceValue([...sequence, next]));
        const nextsValuesUnsureOrWinningForLastPlayer = nextsValues.filter(v => !nextPlayer.isWinning(v));
        const thereIsSomeNextValueWinningForNextPlayer = nextsValues.some(v => nextPlayer.isWinning(v));
        if (thereIsSomeNextValueWinningForNextPlayer) {
             return checkNotNan(nextPlayer.attenuate(nextPlayer.getBestValue(nextsValues), nextsValuesUnsureOrWinningForLastPlayer.length + 1));
        }
        // only unsure or winning for last player
        if (nextsValues.includes(lastPlayer.value)) {
            const nextsValuesUnsureOrWinningForNextPlayer = nextsValues.filter(_ => _ != lastPlayer.value);
            if (nextsValuesUnsureOrWinningForNextPlayer.length === 0) {
                return checkNotNan(lastPlayer.value);
            } else {
                // some moves are winning for lastPlayer, but nextPlayer is unlikely to play them.
                // just add a few points for lastPlayer.
                return checkNotNan(lastPlayer.attenuate(nextPlayer.getBestValue(nextsValuesUnsureOrWinningForNextPlayer),
                    nextsValues.length - nextsValuesUnsureOrWinningForNextPlayer.length));
            }
        }
        return checkNotNan(nextPlayer.minmax(nextsValues));
    }

    evaluateAllSubsequences() {
        const moves = this.game.moves;
        const lastPlayerValue = Player.getLastPlayer(moves).value;
        this.sequenceValueStorage.storeValue(moves, lastPlayerValue);
        this.sequenceValueStorage.storeValue(moves.slice(0, moves.length -1), lastPlayerValue);
        range(moves.length - 2, 1).reverse().map(_ => moves.slice(0, _)).forEach((subsequence) => {
            const value = this.evaluateSequence(subsequence);
            if (Number.isNaN(value)) {
                throw new Error();
            }
            if (value !== unsure) {
              this.sequenceValueStorage.storeValue(subsequence, value);
            } else {
                this.sequenceValueStorage.removeValue(subsequence);
            }
        });
    }

    getSequenceValue(sequence) {
        const storedValue = this.sequenceValueStorage.getValue(sequence);
        if (storedValue !== null && storedValue !== undefined) {
            return Number(storedValue);
        }
        return unsure;
    }

    evaluateNexts(time) {
        this.evaluateAbstract(time, (gameCopy, time) => {setTimeout(() => new Evaluator(gameCopy, null, this.sequenceValueStorage).evaluateNexts(time), 0);});
    }

    evaluateNextsSync(time) {
        this.evaluateAbstract(time, (gameCopy, time) => {new Evaluator(gameCopy, null, this.sequenceValueStorage).evaluateNextsSync(time);});
    }

    evaluateAbstract(time, f) {
        if (!time || this.game.clock.getTime() < time) {
            const value = this.getSequenceValue(this.game.moves);
            if (!firstPlayer.isWinning(value) && !secondPlayer.isWinning(value)) {
                for (let next of this.game.possibleNexts) {
                    const gameCopy = this.game.copy();
                    gameCopy.play(next);
                    f(gameCopy, time);
                }
            }
        }
    }

    // choose best next move for bot who plays second
    chooseNext() {
        const possibleNextsValues = this.game.possibleNexts.map(move => {return {move, value:this.getMoveValue(move)};});
        console.debug('possibleNextsValues', possibleNextsValues);
        const winning = possibleNextsValues.find(_ => _.value < probableSecondPlayer);
        if (winning) {
            console.debug('winning', winning);
            return winning.move;
        }
        const notLosings = possibleNextsValues.filter(_ => _.value <= probableFirstPlayer);
        console.debug('notLosings', notLosings);
        if (notLosings.length >= 1) {
            const weighteds = notLosings.map(_ => {
                return {move: _.move, weight: otherPlayerValue(_.value)};
            });
            console.debug('weighteds', weighteds);
            return (pickWeighted(weighteds)).move;
        } else {
            return pick(argsMin(possibleNextsValues, _ => _.value)).move;
        }
    }

    getMoveValue(nextId) {
        return this.getSequenceValue([...this.game.moves, nextId]);
    }
    
    onGameOver(winner) {
        this.evaluateAllSubsequences();
        this.gameOverCallback(winner);
    }

}

class LocalStorageSequenceValueStorage {
    storeValue(sequence, value) {
        if (Number.isNaN(value)) {
            throw new Error();
        }
        localStorage.setItem(sequence, value);
    }
    getValue(sequence) {
        return localStorage.getItem(sequence);
    }
    removeValue(sequence) {
        localStorage.removeItem(sequence);
    }
}
