"use strict";

let previous;
let current;
const firstPlayer = 1000;
const secondPlayer = 0;
const probableFirstPlayer = 750;
const probableSecondPlayer = 250;
const unsure = (probableFirstPlayer + probableSecondPlayer) / 2;
const sequenceValuesMap = new Map([
    [probableSecondPlayer, probableSecondPlayer],
    [(probableSecondPlayer + unsure) / 2 , probableSecondPlayer],
    [unsure, unsure],
    [(unsure + probableFirstPlayer) / 2, probableFirstPlayer], 
    [probableFirstPlayer, probableFirstPlayer]]);

function range(size, startAt = 0) {
    return [...Array(size).keys()].map(i => i + startAt);
}

function getRandomInt(min, max) {
  return Math.floor(Math.random() * (max - min) + min);
}

function pick(array) {
    return array[getRandomInt(0, array.length)];
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

function max(array, f) {
    return array.reduce((acc, cur) => {
        const currentValue = f(cur);
        if (currentValue > acc.value) {
            return {elements: [cur], value: currentValue};
        } else if (currentValue === acc.value) {
            return {elements: [...acc.elements, cur], value: currentValue};
        } else {
            return acc;
        }
    },
    {value: -Number.MAX_VALUE});
}

function argsMin(array, f) {
    return min(array, f).elements;
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

class Game {
    constructor(nextss, sequenceValueStorage) {
        this.nextss = nextss;
        this.moves = [];
        this.possibleNexts = undefined;
        this.winnings = new Set();
        this.losings = new Set();
        this.initialNextss = JSON.parse(JSON.stringify(nextss));
        this.sequenceValueStorage= sequenceValueStorage;
    }

    getCurrentMove() {
        return this.moves[this.moves.length - 1];
    }

    getPreviousMove() {
        return this.moves[this.moves.length - 2];
    }

    getCurrentPlayer(moves) {
        return moves.length % 2 * firstPlayer;
    }

    evaluateSequence(sequence) {
        const lastMove = sequence[sequence.length - 1];
        // player who just played last move
        const lastPlayer = getLastPlayer(sequence);
        const nextPlayer = otherPlayer(lastPlayer);
        const nexts = this.initialNextss[lastMove].filter(_ => !sequence.includes(_));
        const nextsValues = new Set(nexts.map(next => this.getSequenceValue([...sequence, next])));
        if (nextsValues.has(nextPlayer)) {
            return nextPlayer;
        }
        if (nextsValues.has(lastPlayer)) {
            if (nextsValues.size === 1) {
                return lastPlayer;
            } else {
                return probablePlayer(lastPlayer);
            }
        }
        const entry = average([...nextsValues])
        if (!sequenceValuesMap.has(entry)) {
            throw new Error('absent nextsValues', entry);
        }
        return sequenceValuesMap.get(entry);
    }

    evaluateAllSubsequences() {
        this.sequenceValueStorage.storeValue(this.moves, getLastPlayer(this.moves));
        this.sequenceValueStorage.storeValue(this.moves.slice(0, this.moves.length -1), getLastPlayer(this.moves));
        range(this.moves.length - 2, 1).reverse().map(_ => this.moves.slice(0, _)).forEach((subsequence) => {
            const value = this.evaluateSequence(subsequence);
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

    // choose best next move for bot who plays second
    chooseNext() {
        console.debug('this.possibleNexts', this.possibleNexts);
        console.debug('this.winnings', this.winnings);
        const winning = this.possibleNexts.find(id => this.winnings.has(id));
        console.debug('winning', winning);
        if (winning) {
            this.sequenceValueStorage.storeValue(this.moves, secondPlayer);
            return winning;
        }
        console.debug('this.losings', this.losings);
        const notLosings = this.possibleNexts.filter(id => !this.losings.has(id) && this.evaluateMove(id) != firstPlayer);
        console.debug('notLosings', notLosings);
        if (notLosings.length >= 1) {
            const weighteds = notLosings.map(move => {
                return {move, weight: otherPlayer(this.evaluateMove(move))};
            });
            console.debug('weighteds', weighteds);
            return (pickWeighted(weighteds)).move;
        } else {
            this.sequenceValueStorage.storeValue(this.moves, firstPlayer);
            this.sequenceValueStorage.storeValue(this.moves.slice(0, this.moves.length -1), firstPlayer);
            return pick(this.possibleNexts);
        }
    }

    // add winnings and losings while updating nextss
    takeWinnings(nexts, iNexts) {
        if (iNexts != this.getCurrentMove() && nexts.length === 1) {
            this.winnings.add(iNexts.toString());
            let prevVertex = iNexts;
            let curVertex = nexts[0];
            let vertices = this.losings;
            while ((this.nextss[curVertex].length === 2) && (curVertex != this.getCurrentMove())) {
                vertices.add(curVertex);
                const nextVertex = this.nextss[curVertex].find(_ => _ != prevVertex);
                prevVertex = curVertex;
                curVertex = nextVertex;
                vertices = [this.winnings, this.losings].find (_ => _ !== vertices);
            }
            vertices.add(curVertex);
        }
        return nexts;
    }

    play(current) {
        this.moves.push(current);
        const previous = this.getPreviousMove(); 
        if (previous !== undefined) {
            this.winnings.clear();
            this.losings.clear();
            this.nextss = this.nextss.map((nexts, iNexts) => {
                return this.takeWinnings(nexts.filter(next => next != previous), iNexts);
            });
        }
        // a vertex both winning and losing is actually losing
        this.losings.forEach(losing => this.winnings.delete(losing));
        this.possibleNexts = [...this.nextss[current]];
        this.nextss[current] = [];
        if (this.possibleNexts.length === 0) {
            // the game is over
            this.evaluateAllSubsequences();
        }
    }

    evaluateMove(nextId) {
        return this.getSequenceValue([...this.moves, nextId]);
    }
}

class LocalStorageSequenceValueStorage {
    storeValue(sequence, value) {
        localStorage.setItem(sequence, value);
    }
    getValue(sequence) {
        return localStorage.getItem(sequence);
    }
    removeValue(sequence) {
        localStorage.removeItem(sequence);
    }
}

function getLastPlayer(moves) {
    return moves.length % 2 * firstPlayer;
}

function otherPlayer(player) {
    return firstPlayer - player;
}

function probablePlayer(player) {
    return (unsure + player) / 2;
}

function enlargeVertices() {
    document.querySelectorAll('g.node').forEach(g => {
        // all vertices seem to be at least 18 away from each other.
        const extraRadius = 9;
        const ellipse = g.querySelector('ellipse');
        const clonedEllipse = ellipse.cloneNode(false);
        clonedEllipse.removeAttribute('stroke');
        clonedEllipse.setAttribute('rx', Number(clonedEllipse.getAttribute('rx')) + extraRadius);
        clonedEllipse.setAttribute('ry', Number(clonedEllipse.getAttribute('ry')) + extraRadius);
        ellipse.insertAdjacentElement('beforebegin', clonedEllipse);
    });
}

document.addEventListener('DOMContentLoaded', function() {
    const game = new Game(nextss, new LocalStorageSequenceValueStorage());
    enlargeVertices();

    function getNodeId(node) {
        return node.id.substring(2);
    }

    function play(tmp) {
        previous = current;
        current = tmp;
        game.play(getNodeId(current));
        updateSvgCurrentVertex();
        if (previous !== undefined) {
            updateSvgLastVertex(previous);
        }
    }

    function updateSvgCurrentVertex() {
        let ellipse = current.querySelector('ellipse + ellipse');
        ellipse.setAttribute('fill', 'gray');
    }

    function updateSvgLastVertex(previous) {
        document.querySelectorAll(`g._${game.getPreviousMove()}`).forEach(g => {
            if (g.getAttribute('class').includes(`_${game.getCurrentMove()}`)) {
                g.querySelector('path').setAttribute('stroke', 'lightgray');
            } else if (game.moves.length < 3 || !g.getAttribute('class').includes(`_${game.moves[game.moves.length - 3]}`)) {
                g.remove();
            }
        });
        const lastCircle = previous.querySelector('ellipse + ellipse');
        lastCircle.setAttribute('stroke', 'lightgray');
        lastCircle.setAttribute('fill', 'none');
        if (game.moves.length === 2) {
            lastCircle.setAttribute('stroke-width', '3');
        }
        previous.querySelector('text').setAttribute('style', 'fill: lightgray;');
    }

    document.body.onclick = (event) => {
        const tmp = event.target.closest('svg > g > g.node');
        if (tmp && (game.possibleNexts === undefined || game.possibleNexts.includes(getNodeId(tmp)))) {
            play(tmp);
            if (game.possibleNexts.length > 0) {
                const botChoice = game.chooseNext();
                const botElement = document.querySelector(`g#id${botChoice}`);
                setTimeout(() => {play(botElement);}, 1000);
            }
        }
    }
});

