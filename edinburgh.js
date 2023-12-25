"use strict";

import {Combination, combination, randomInteger} from 'https://cdn.jsdelivr.net/npm/js-combinatorics@2.1.2/combinatorics.min.js';
import { instance as viz } from "https://cdn.jsdelivr.net/npm/@viz-js/viz@3.2.3/lib/viz-standalone.min.js";

function range(size, startAt = 0) {
    return [...Array(size).keys()].map(i => i + startAt);
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
    const vertices = range(9);
    const edges = [...Combination(vertices, 2)];
    const graphs = Combination.of(edges, 13);
    const graph = graphs.at(randomInteger(combination(36, 13)));
    
    enlargeVertices();
    let last;
    let current;
    let possibleIds;
    document.body.onclick = (event) => {
        const tmp = event.target.closest('svg > g > g.node');
        if (tmp && (possibleIds === undefined || possibleIds.includes(tmp.id))) {
            last = current;
            current = tmp;
            let ellipse = current.querySelector('ellipse + ellipse')
            ellipse.setAttribute('fill', 'gray');
            if (last !== undefined) {
                document.querySelectorAll(`g._${last.id.substring(2)}`).forEach(_ => _.remove());
                last.remove();
            }
            possibleIds = nextss[current.id];
        }
    }
});
    
