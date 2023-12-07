"use strict";

//deep clone
let initialNextss = JSON.parse(JSON.stringify(nextss));
let last;
let current;
let currentId;
let possibleIds;
let winnings = new Set();
let losings = new Set();
const game = [];
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

function pickWeighted(weighteds) {
    const summedWeights = weighteds.reduce((acc, cur) => {
        acc.push((acc[acc.length - 1] ?? 0) + cur.weight);
        return acc;
    }, []);
    const r = Math.random() * summedWeights[summedWeights.length - 1];
    return weighteds[summedWeights.findIndex(aw => aw >= r)];
}

function getNodeId(node) {
    return node.id.substring(2);
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

function getLastPlayer(game) {
    return game.length % 2 * firstPlayer;
}

function otherPlayer(player) {
    return firstPlayer - player;
}

function probablePlayer(player) {
    return (unsure + player) / 2;
}

function sum(array) {
    return array.reduce((acc, cur) => acc + cur);
}

function average(array) {
    return sum(array) / array.length;
}

function evaluateSequence(sequence) {
    const lastMove = sequence[sequence.length - 1];
    // player who just played last
    const lastPlayer = getLastPlayer(sequence);
    const nextPlayer = otherPlayer(lastPlayer);
    const nexts = initialNextss[lastMove].filter(_ => !sequence.includes(_));
    const nextsValues = new Set(nexts.map(next => getSequenceValue([...sequence, next])));
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

function evaluateSubsequences(sequence) {
    localStorage.setItem(game, getLastPlayer(game));
    localStorage.setItem(game.slice(0, game.length -1), getLastPlayer(game));
    range(sequence.length - 1, 1).reverse().map(_ => sequence.slice(0, _)).forEach((subsequence) => {
        const value = evaluateSequence(subsequence);
        if (value !== unsure) {
            localStorage.setItem(subsequence, value);
        } else {
            localStorage.removeItem(subsequence);
        }
    });
}

function getSequenceValue(sequence) {
    const storedValue = localStorage.getItem(sequence);
    if (storedValue !== null) {
        return Number(storedValue);
    }
    return unsure;
}

function evaluateMove(nextId) {
    return getSequenceValue([...game, nextId]);
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
    enlargeVertices();
    // add winnings and losings while updating nextss
    function takeWinnings(nexts, iNexts) {
        if (iNexts != currentId && nexts.length === 1) {
            winnings.add(iNexts.toString());
            losings.add(nexts[0]);
        }
        return nexts
    }

    function chooseNext() {
        const gam = game.slice(0, game.length -1);
        const winning = possibleIds.find(id => winnings.has(id));
        if (winning) {
            localStorage.setItem(game, secondPlayer);
            return winning;
        }
        const notLosings = possibleIds.filter(id => !losings.has(id) && evaluateMove(id) != firstPlayer);
        if (notLosings.length >= 1) {
            return pickWeighted(notLosings.map(_ => {return {move: _, weight: otherPlayer(evaluateMove(_))};})).move;
        }
        localStorage.setItem(game, firstPlayer);
        localStorage.setItem(gam, firstPlayer);
        return pick(possibleIds);
    }

    function play(tmp) {
        last = current;
        current = tmp;
        currentId = getNodeId(current);
        game.push(currentId);
        updateSvgCurrentVertex();
        if (last !== undefined) {
            const lastId = getNodeId(last);
            updateSvgLastVertex(lastId);
            winnings.clear();
            losings.clear();
            nextss = nextss.map((nexts, iNexts) => {
                return takeWinnings(nexts.filter(next => next !== lastId), iNexts);
            });
        }
        possibleIds = [...nextss[currentId]];
        nextss[currentId] = [];
        if (possibleIds.length === 0) {
            evaluateSubsequences(game);
        }
    }

    function updateSvgCurrentVertex() {
        let ellipse = current.querySelector('ellipse + ellipse');
        ellipse.setAttribute('fill', 'gray');
    }

    function updateSvgLastVertex(lastId) {
        document.querySelectorAll(`g._${lastId}`).forEach(g => {
            if (g.getAttribute('class').includes(`_${currentId}`)) {
                g.querySelector('path').setAttribute('stroke', 'lightgray');
            } else if (game.length < 3 || !g.getAttribute('class').includes(`_${game[game.length - 3]}`)) {
                g.remove();
            }
        });
        const lastCircle = last.querySelector('ellipse + ellipse');
        lastCircle.setAttribute('stroke', 'lightgray');
        lastCircle.setAttribute('fill', 'none');
        if (game.length === 2) {
            lastCircle.setAttribute('stroke-width', '3');
        }
        last.querySelector('text').setAttribute('style', 'fill: lightgray;');
    }

    document.body.onclick = (event) => {
        const tmp = event.target.closest('svg > g > g.node');
        if (tmp && (possibleIds === undefined || possibleIds.includes(getNodeId(tmp)))) {
            play(tmp);
            if (possibleIds.length > 0) {
                const botChoice = chooseNext();
                const botElement = document.querySelector(`g#id${botChoice}`);
                setTimeout(() => {play(botElement);}, 1000);
            }
        }
    }
});


