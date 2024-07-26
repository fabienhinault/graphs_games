"use strict";

import {firstPlayer, secondPlayer, Game, Evaluator, LocalStorageSequenceValueStorage, Clock} from './model.js';
import { voronoize } from './voronoize.js';

function moveEdgesLast() {
    document.querySelectorAll('#graph0 > g.edge').forEach(g => {
        g.parentElement.appendChild(g);
    });
}

function onGameOver(winnerName) {
    const klass = `${winnerName}_won`;
    ['#robot_won', '#player_won']
        .map(idSelector => document.querySelector(idSelector))
        .forEach(img => img.setAttribute('class', klass));
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
    const tmpSvg = document.querySelector('#svg');
    tmpSvg.innerHTML = await (await fetch(`${path}.svg`)).text();
    tmpSvg.replaceWith(...tmpSvg.childNodes);
    let game = new Game(nextss, JSON.parse(JSON.stringify(nextss)), new Clock(), null);
    let current;
    let previous;
    const evaluator = new Evaluator(game, onGameOver, new LocalStorageSequenceValueStorage(), null);
    game.gameOverCallback = evaluator.onGameOver.bind(evaluator);
    const graphElement = document.querySelector('#graph0');
    const parentSvg = graphElement.closest('svg');
    moveEdgesLast();
    voronoize(parentSvg, graphElement);

    function play(tmp) {
        previous = current;
        current = tmp;
        const idNumber = getNodeId(current);
        game.play(idNumber);
        evaluator.pushValue();
        updateSvgCurrentVertex(current);
        if (previous !== undefined) {
            updateSvgPreviousVertex(previous, game);
        }
    }

    function robotPlays() {
        const botChoice = evaluator.chooseNext();
        const botElement = document.querySelector(`g#id${botChoice}`);
        play(botElement);
        document.body.addEventListener('click', onClick);
        console.debug(evaluator.getSequenceValue(game.moves));
        console.debug(localStorage.length);
    }

    function onClick(event) {
        document.body.removeEventListener('click', onClick);
        const tmp = event.target.closest('svg > g > g.node');
        if (tmp && (game.possibleNexts === undefined || game.possibleNexts.includes(getNodeId(tmp)))) {
            play(tmp);
            if (game.possibleNexts.length > 0) {
                evaluator.evaluateNexts(game.clock.getTime() + 900);
                setTimeout(robotPlays, 1000);
            }
        }
    }

    function onFirstClick(event) {
        firstPlayer.name = 'player';
        secondPlayer.name = 'robot';
        document.querySelector('button#robot_begins').setAttribute('class', 'started');
        evaluator.player = secondPlayer;
        onClick(event);
    }

    document.body.addEventListener('click', onFirstClick);

    document.querySelector('#robot_begins').onclick = (event) => {
        firstPlayer.name = 'robot';
        secondPlayer.name = 'player';
        document.querySelector('button#robot_begins').setAttribute('class', 'started');
        evaluator.player = firstPlayer;
        document.body.removeEventListener('click', onFirstClick);
        event.stopPropagation();
        robotPlays();
    }
});


