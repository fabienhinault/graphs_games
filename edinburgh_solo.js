"use strict";

let last;
let current;
let currentId;
let possibleIds;
let winnings = new Set();
let losings = new Set();
const game = [];


function getRandomInt(min, max) {
  return Math.floor(Math.random() * (max - min) + min);
}

function pick(array) {
    return array[getRandomInt(0, array.length)];
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

function evaluatePlay(nextId) {
    const storedValue = localStorage.getItem([...game, nextId]);
    if (storedValue !== null) {
        return storedValue;
    }
    return 0.5;
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
            localStorage.setItem(game, 0);
            return winning;
        }
        const notLosings = possibleIds.filter(id => !losings.has(id));
        if (notLosings.length === 1) {
            return notLosings[0];
        }
        if (notLosings.length > 1) {
            return pick(argsMin(notLosings, evaluatePlay));
        }
        localStorage.setItem(game, 1);
        localStorage.setItem(gam, 1);
        return pick(possibleIds);
    }

    function play(tmp) {
        last = current;
        current = tmp;
        currentId = getNodeId(current);
        game.push(currentId);
        let ellipse = current.querySelector('ellipse + ellipse')
        ellipse.setAttribute('fill', 'gray');
        if (last !== undefined) {
            const lastId = getNodeId(last);
            document.querySelectorAll(`g._${lastId}`).forEach(_ => _.remove());
            last.remove();
            winnings.clear();
            losings.clear();
            nextss = nextss.map((nexts, iNexts) => {
                return takeWinnings(nexts.filter(next => next !== lastId), iNexts);
            });
        }
        possibleIds = [...nextss[currentId]];
        nextss[currentId] = [];
        if (possibleIds.length === 0) {
            // the values are: 1 if the first player wins, 0 if the second player wins.
            // As the bot always plays second, 0 are winning games, 1 are losing games.
            localStorage.setItem(game, game.length % 2);
        }
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

