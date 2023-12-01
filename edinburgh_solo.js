"use strict";

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
    let last;
    let current;
    let possibleIds;
    let winnings = new Set();
    let losings = new Set();

    // add winnings and losings while updating nextss
    function takeWinnings(nexts, iNexts) {
        if (nexts.length === 1) {
            winnings.add(iNexts.toString());
            losings.add(nexts[0]);
        }
        return nexts
    }

//    function chooseNext() {
//        if 
    function play(tmp) {
        last = current;
        current = tmp;
        let ellipse = current.querySelector('ellipse + ellipse')
        ellipse.setAttribute('fill', 'gray');
        if (last !== undefined) {
            document.querySelectorAll(`g._${getNodeId(last)}`).forEach(_ => _.remove());
            last.remove();
        }
        const currentId = getNodeId(current);
        nextss = nextss.map((nexts, iNexts) => takeWinnings(nexts.filter(next => next !== currentId), iNexts));
        possibleIds = nextss[currentId];
    }

    document.body.onclick = (event) => {
        const tmp = event.target.closest('svg > g > g.node');
        if (tmp && (possibleIds === undefined || possibleIds.includes(getNodeId(tmp)))) {
            play(tmp);
            if (possibleIds.length > 0) {
                const botChoice = pick(possibleIds);
                const botElement = document.querySelector(`g#id${botChoice}`);
                setTimeout(() => {play(botElement);}, 1000);
            }
        }
    }
});
    
