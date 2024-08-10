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

function optimum(array, f, compare, initialValue) {
    return array.reduce((acc, cur) => {
        const currentValue = f(cur);
        if (compare(currentValue, acc.value)) {
            return {elements: [cur], value: currentValue};
        } else if (currentValue === acc.value) {
            return {elements: [...acc.elements, cur], value: currentValue};
        } else {
            return acc;
        }
    },
    {value: initialValue});
}

function argsOpt(array, f, compare, initialValue) {
    return optimum(array, f, compare, initialValue).elements;
}

function min(array, f) {
    return optimum(array, f, (x,y) => x < y, Number.MAX_VALUE)
}

function argsMin(array, f) {
    return min(array, f).elements;
}

function max(array, f) {
    return optimum(array, f, (x,y) => x > y, -Number.MAX_VALUE)
}

function argsMax(array, f) {
    return max(array, f).elements;
}

export class Clock {
    getTime() {
        return Date.now();
    }
}

const firstPlayerValue = 1000;
const secondPlayerValue = 0;
const unsure = (firstPlayerValue + secondPlayerValue) / 2;
const valueThreshold = Math.abs((firstPlayerValue - secondPlayerValue) / 4);

class Player {
    constructor(value, bestValueFunction, attenuationFactor, argsBestFunction, straightenFunction) {
        this.name = '';
        // firstPlayerValue or secondPlayerValue
        this.value = value;
        this.bestValueFunction = bestValueFunction;
        this.attenuationFactor = attenuationFactor;
        this.argsBestFunction = argsBestFunction;
        this.straightenFunction = straightenFunction;
    }
    
    getOtherPlayer() {
        return otherPlayers[players.indexOf(this)];
    }
    
    static getLastPlayer(moves) {
        return players[(moves.length % 2)];
    }
    
    getProbable() {
        return (unsure + this.value) / 2;
    }

    getBestValue(nextsValues) {
        return this.bestValueFunction(...nextsValues);
    }

    getArgsBest(moves, f) {
        return this.argsBestFunction(moves, f);
    }

    // attenuate the value to be given to a sequence.
    // Typically the sequence will be given the value of the player attenuated to take
    // some circumstances in account.
    // For example a sequence with one winning next move and many losing next moves is not as winning as
    // a sequence with only winning next moves and no losing next move.
    attenuate(value, weight) {
        return value - this.attenuationFactor * weight; 
    }

    // wether the value of a sequence is winning for player
    isWinning(sequenceValue) {
        return Math.abs(sequenceValue - this.value) < valueThreshold;
    }

    getStraightenedValue(sequenceValue) {
        return this.straightenFunction(sequenceValue);
    }
}

const firstAttenuationFactor = 0.02 * (firstPlayerValue - unsure);
export const firstPlayer = new Player(firstPlayerValue, Math.max, firstAttenuationFactor, argsMax, x => x);
export const secondPlayer = new Player(secondPlayerValue, Math.min, -firstAttenuationFactor, argsMin, x => firstPlayerValue - x);
let players = [secondPlayer, firstPlayer];
let otherPlayers = [firstPlayer, secondPlayer];

function otherPlayerValue(playerValue) {
    return firstPlayerValue - playerValue;
}

function checkNotNan(n) {
    if (Number.isNaN(n)) {
        throw new Error();
    }
    return n;
}

export class Game {
    constructor(nextss, initialNextss, clock, eventDispatcher) {
        this.nextss = nextss;
        this.moves = [];
        this.possibleNexts = range(nextss.length);
        this.initialNextss = initialNextss;
        this.clock = clock;
        this.currentPlayer = firstPlayer;
        this.dispatcher = eventDispatcher;
        this.evaluator;
    }

    cloneNextss() {
        return [...this.nextss.map(_ => [..._])];
    }

    dispatch(type, detail) {
        // this.dispatcher is null for game copies used by evaluators
        this.dispatcher?.dispatchEvent(new CustomEvent(type, {detail: detail}));
    }

    addGameOverListener(listener) {
        this.addListener('game over', listener);
    }

    addPlayedListener(listener) {
        this.addListener('played', listener);
    }
    
    addListener(eventType, listener) {
        this.dispatcher.addEventListener(eventType, listener);
    }

    copy() {
        const result = new Game(this.cloneNextss(), this.initialNextss, this.clock, null);
        result.moves = [...this.moves];
        return result;
    }

    getCurrentMove() {
        return this.moves[this.moves.length - 1];
    }

    getPreviousMove() {
        return this.moves[this.moves.length - 2];
    }

    getPrepreviousMove() {
        return this.moves[this.moves.length - 3];
    }

    getCurrentPlayer() {
        return (this.moves.length % 2) * firstPlayer;
    }

    updateNextss(current) {
        this.nextss = this.nextss.map((nexts, iNexts) => {
            return nexts.filter(next => next != current);
        });
        this.nextss[current] = [];
    }

    play(current) {
        this.moves.push(current);
        this.possibleNexts = [...this.nextss[current]];
        this.updateNextss(current);
        this.dispatch("played", {move: this.getLastMove()});
        if (this.possibleNexts.length === 0) {
            // the game is over
            // this.evaluator is null in a 2 player game
            this.evaluator?.onGameOver(this.currentPlayer.name);
            this.dispatch("game over", {winner: this.currentPlayer.name});
        }
        this.currentPlayer = this.currentPlayer.getOtherPlayer();
    }

    getLastMove() {
        return this.moves[this.moves.length - 1];
    }

    getNextMoves() {
        return this.nextss[this.getLastMove()];
    }
}


export class Evaluator {
    constructor(game, sequenceValueStorage, player) {
        this.game = game;
        this.game.evaluator = this;
        this.sequenceValueStorage = sequenceValueStorage;
        this.player = player ?? secondPlayer;
        if (this.game.dispatcher) {
            this.game.addGameOverListener(evt => this.onGameOver(evt.winner));
        }
    }

    /* The value of a sequence says if the sequence is winning for firstPlayer or secondPlayer.
     * It is winning for firstPlayer if the value is close to firstPlayerValue, and same for secondPlayer.
     * The value of the sequence depends on the value of its successors.
     * All players are considered good. If a winning move exists, the evaluation supposes she will take it.
     */
    evaluateSequence(sequence) {
        const lastMove = sequence[sequence.length - 1];
        // player who just played last move
        const lastPlayer = Player.getLastPlayer(sequence);
        const nextPlayer = lastPlayer.getOtherPlayer();
        const nexts = this.game.initialNextss[lastMove].filter(_ => !sequence.includes(_));
        const nextsValues = nexts.map(next => this.getSequenceValue([...sequence, next]));
        const bestValue = nextPlayer.getBestValue(nextsValues);
        if (nextPlayer.isWinning(bestValue)) {
            // the current sequence is considered winning for nextPlayer.
            // attenuated for the possible losing moves, and the distance.
            return checkNotNan(nextPlayer.attenuate(bestValue, nextsValues.filter(v => !nextPlayer.isWinning(v)).length + 1));
        }
        if (lastPlayer.isWinning(bestValue)) {
            return checkNotNan(lastPlayer.attenuate(bestValue, nextsValues.filter(v => !lastPlayer.isWinning(v)).length + 1));
        }
        return checkNotNan(bestValue);
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
        this.evaluateAbstract(time, (evaluator, t) => {
            setTimeout(() => {
                evaluator.evaluateNexts(t);
            }, 0);
        });
    }

    evaluateNextsSync(time) {
        this.evaluateAbstract(time, (evaluator, t) => {
            evaluator.evaluateNextsSync(t);
        });
    }

    evaluateAbstract(time, f) {
        if (!time || this.game.clock.getTime() < time) {
            for (let next of this.game.possibleNexts) {
                const gameCopy = this.game.copy();
                const evaluator = new Evaluator(gameCopy, this.sequenceValueStorage, this.player);
                gameCopy.gameOverCallback = evaluator.onGameOver.bind(evaluator); 
                gameCopy.play(next);
                f(evaluator, time);
            }
        }
    }

    // choose best next move for bot who plays second
    chooseNext() {
        const possibleNextsValues = this.game.possibleNexts.map(move => {return {move, value:this.getMoveValue(move)};});
        const winning = possibleNextsValues.find(mv => this.player.isWinning(mv.value));
        if (winning) {
            return pick(this.player.getArgsBest(possibleNextsValues, _ => _.value)).move;
        }
        const otherPlayer = this.player.getOtherPlayer()
        const notLosings = possibleNextsValues.filter(mv => !otherPlayer.isWinning(mv.value));
        if (notLosings.length >= 1) {
            const weighteds = notLosings.map(_ => {
                return {move: _.move, weight: this.player.getStraightenedValue(_.value)};
            });
            return (pickWeighted(weighteds)).move;
        } else {
            return pick(this.player.getArgsBest(possibleNextsValues, _ => _.value)).move;
        }
    }

    getMoveValue(nextId) {
        return this.getSequenceValue([...this.game.moves, nextId]);
    }
    
    onGameOver(winner) {
        this.evaluateAllSubsequences();
    }

}

export class LocalStorageSequenceValueStorage {
    storeValue(sequence, value) {
        if (Number.isNaN(value)) {
            throw new Error();
        }
        try {
            localStorage.setItem(sequence, value);
        } catch (e) {
            localStorage.clear();
        }
    }
    getValue(sequence) {
        return localStorage.getItem(sequence);
    }
    removeValue(sequence) {
        localStorage.removeItem(sequence);
    }
}

