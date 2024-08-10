"use strict";

import {firstPlayer, secondPlayer, Game, Evaluator, LocalStorageSequenceValueStorage, Clock} from './model.js';
import { voronoize } from './voronoize.js';
import {View} from './view.js';

function getNodeId(svgNode) {
    // remove "id"
    return Number(svgNode.id.substring(2));
}

document.addEventListener('DOMContentLoaded', async function() {
    const path = new URL(window.location.toLocaleString()).searchParams.get('path');
    const {nextss} = await import(`${path}.js`);
    const svgContent = await fetch(`${path}.svg`);
    const svgHtml = await svgContent.text();
    let game;
    let view;
    init();

    function init(evt) {
        evt?.stopPropagation();
        document.body.removeEventListener('click', init);
        const tmpSvg = document.querySelector('body > svg');
        tmpSvg.innerHTML = svgHtml;
        tmpSvg.replaceWith(...tmpSvg.childNodes);
        if (game) {
            game.dispatcher.removeEventListener('game over', onGameOver);
        }
        game = new Game(nextss, JSON.parse(JSON.stringify(nextss)), null, document);
        game.addGameOverListener(onGameOver);
        if (view) {
            // else, the old view continues listening
            view.stopListening();
        }
        view = new View(game);
        firstPlayer.name = 'player1';
        secondPlayer.name = 'player2';
        const graphElement = document.querySelector('#graph0');
        graphElement.querySelector('g#robot_begins').remove();
        document.body.addEventListener('click', onClick);
    }

    function onGameOver(evt) {
        document.body.removeEventListener('click', onClick);
        document.body.addEventListener('click', init);
    }

    function onClick(event) {
        const tmp = event.target.closest('g#graph0 > g.node');
        if (tmp && (game.possibleNexts === undefined || game.possibleNexts.includes(getNodeId(tmp)))) {
            game.play(getNodeId(tmp));
        }
    }
});
    
