"use strict";

import {firstPlayer, secondPlayer, Game, Evaluator, LocalStorageSequenceValueStorage, Clock} from './model.js';
import { voronoize } from './voronoize.js';

function moveEdgesLast() {
    document.querySelectorAll('#graph0 > g.edge').forEach(g => {
        g.parentElement.appendChild(g);
    });
}

function onGameOver(winnerName) {
    setTimeout(() => {
        const klass = `${winnerName}_won`;
        ['#player1_won', '#player2_won']
            .map(idSelector => document.querySelector(idSelector))
            .forEach(img => img.setAttribute('class', klass));
    }, 1000);
}

function getNodeId(svgNode) {
    // remove "id"
    return Number(svgNode.id.substring(2));
}

function updateSvgCurrentVertex(currentSvgNode) {
    let ellipse = currentSvgNode.querySelector('ellipse');
    ellipse.setAttribute('fill', 'gray');
}

function updateSvgPreviousVertex(previousSvgNode, game) {
    document.querySelectorAll(`g._${getNodeId(previousSvgNode)}_`).forEach(g => {
        if (g.getAttribute('class').includes(`_${game.getCurrentMove()}_`)) {
            g.querySelector('path').setAttribute('stroke', 'lightgray');
        } else if (game.moves.length < 3 || !g.getAttribute('class').includes(`_${game.moves[game.moves.length - 3]}_`)) {
            g.remove();
        }
    });
    const lastCircle = previousSvgNode.querySelector('ellipse');
    lastCircle.setAttribute('stroke', 'lightgray');
    lastCircle.setAttribute('fill', 'none');
    if (game.moves.length === 2) {
        lastCircle.setAttribute('stroke-width', '3');
    }
    previousSvgNode.querySelector('text').setAttribute('style', 'fill: lightgray;');
}

document.addEventListener('DOMContentLoaded', async function() {
    const path = new URL(window.location.toLocaleString()).searchParams.get('path');
    const {nextss} = await import(`${path}.js`);
    const svg = document.querySelector('#svg');
    svg.innerHTML = await (await fetch(`${path}.svg`)).text();
    svg.replaceWith(...svg.childNodes);
    let game = new Game(nextss, JSON.parse(JSON.stringify(nextss)),null, onGameOver);
    firstPlayer.name = 'player1';
    secondPlayer.name = 'player2';
    const graphElement = document.querySelector('#graph0');
    const parentSvg = graphElement.closest('svg');
    moveEdgesLast();
    voronoize(parentSvg, graphElement);
    let previous;
    let current;

    function play(tmp) {
        previous = current;
        current = tmp;
        game.play(getNodeId(current));
        updateSvgCurrentVertex(current);
        if (previous !== undefined) {
            updateSvgPreviousVertex(previous, game);
        }
    }

    document.body.onclick = (event) => {
        const tmp = event.target.closest('svg > g > g.node');
        if (tmp && (game.possibleNexts === undefined || game.possibleNexts.includes(getNodeId(tmp)))) {
            play(tmp);
        }
    }
});
    
