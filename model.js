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

class Player {
    constructor(value) {
        this.value = value;
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

    minmax(nextsValues) {
        return minmaxes[this.value](...nextsValues);
    }
}

const firstPlayer = new Player(firstPlayerValue);
const secondPlayer = new Player(secondPlayerValue);
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
    constructor(nextss, initialNextss, sequenceValueStorage, clock, gameOverCallback) {
        this.nextss = nextss;
        this.moves = [];
        this.possibleNexts = undefined;
        this.initialNextss = initialNextss;
        this.sequenceValueStorage= sequenceValueStorage;
        this.clock = clock;
        this.gameOverCallback = gameOverCallback;
        this.currentPlayer = firstPlayer;
    }
    
    copy() {
        const result = new Game([...this.nextss.map(_ => [..._])], this.initialNextss, this.sequenceValueStorage, this.clock, null);
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

    evaluateSequence(sequence) {
        const lastMove = sequence[sequence.length - 1];
        // player who just played last move
        const lastPlayer = Player.getLastPlayer(sequence);
        const nextPlayer = lastPlayer.getOtherPlayer();
        const nexts = this.initialNextss[lastMove].filter(_ => !sequence.includes(_));
        const nextsValues = nexts.map(next => this.getSequenceValue([...sequence, next]));
        const nextsValuesNotNextPlayer = nextsValues.filter(_ => Math.abs(_ - nextPlayer.value) > probableSecondPlayer);
        if (nextsValuesNotNextPlayer.length < nextsValues.length) {
             return checkNotNan(nextPlayer.minmax(nextsValues) - 0.02 * (nextPlayer.value - unsure) * (nextsValuesNotNextPlayer.length + 1));
        }
        if (nextsValues.includes(lastPlayer.value)) {
            const nextsValuesNotLastPlayer = nextsValues.filter(_ => _ != lastPlayer.value);
            if (nextsValuesNotLastPlayer.length === 0) {
                return checkNotNan(lastPlayer.value);
            } else {
                // some moves are winning for lastPlayer, but nextPlayer is unlikely to play them.
                // just add a few points for lastPlayer.
                return checkNotNan(nextPlayer.minmax(nextsValuesNotLastPlayer) + 0.02 * (lastPlayer.value - unsure) * (nextsValues.length - nextsValuesNotLastPlayer.length));
            }
        }
        return checkNotNan(nextPlayer.minmax(nextsValues));
    }

    evaluateAllSubsequences() {
        const lastPlayerValue = Player.getLastPlayer(this.moves).value;
        this.sequenceValueStorage.storeValue(this.moves, lastPlayerValue);
        this.sequenceValueStorage.storeValue(this.moves.slice(0, this.moves.length -1), lastPlayerValue);
        range(this.moves.length - 2, 1).reverse().map(_ => this.moves.slice(0, _)).forEach((subsequence) => {
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
        if (this.clock.getTime() < time) {
            for (let next of this.possibleNexts) {
                const game = this.copy();
                game.play(next);
                setTimeout(() => game.evaluateNexts(time), 0);
            }
        }
    }

    evaluateNextsSync(time) {
        if (!time || this.clock.getTime() < time) {
            for (let next of this.possibleNexts) {
                const game = this.copy();
                game.play(next);
                game.evaluateNextsSync(time);
            }
        }
    }


    // choose best next move for bot who plays second
    chooseNext() {
        const possibleNextsValues = this.possibleNexts.map(move => {return {move, value:this.getMoveValue(move)};});
        const winning = possibleNextsValues.find(_ => _.value < probableSecondPlayer);
        if (winning) {
            return winning.move;
        }
        const notLosings = possibleNextsValues.filter(_ => _.value <= probableFirstPlayer);
        if (notLosings.length >= 1) {
            const weighteds = notLosings.map(_ => {
                return {move: _.move, weight: otherPlayerValue(_.value)};
            });
            return (pickWeighted(weighteds)).move;
        } else {
            return pick(argsMin(possibleNextsValues, _ => _.value)).move;
        }
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
            this.evaluateAllSubsequences();
            this.gameOverCallback?.(this.moves.length % 2);
        }
    }

    getMoveValue(nextId) {
        return this.getSequenceValue([...this.moves, nextId]);
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
