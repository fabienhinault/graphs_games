"use strict";

let last;
let current;
let possibleIds;
let winnings = new Set();
let losings = new Set();


function getRandomInt(min, max) {
  return Math.floor(Math.random() * (max - min) + min);
}

function pick(array) {
    return array[getRandomInt(0, array.length)];
}

function getNodeId(node) {
    return node.id.substring(2);
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
        if (nexts.length === 1) {
            winnings.add(iNexts.toString());
            losings.add(nexts[0]);
        }
        return nexts
    }

    function chooseNext() {
        const winning = possibleIds.find(id => winnings.has(id));
        if (winning) {
            return winning;
        }
        return pick(possibleIds.filter(id => !losings.has(id))) ??
            pick(possibleIds);
    }

    function play(tmp) {
        last = current;
        current = tmp;
        const currentId = getNodeId(current);
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
    
