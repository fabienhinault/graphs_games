"use strict";
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
    let prepreviousId;
    let previous;
    let current;
    let possibleIds;

    function getNodeId(node) {
        return node.id.substring(2);
    }

    function play(tmp) {
        if (previous !== undefined) {
            prepreviousId = getNodeId(previous);
        }
        previous = current;
        current = tmp;
        updateSvgCurrentVertex();
        if (previous !== undefined) {
            updateSvgpreviousVertex(previous);
        }
    }

    function updateSvgCurrentVertex() {
        let ellipse = current.querySelector('ellipse + ellipse');
        ellipse.setAttribute('fill', 'gray');
    }

    function updateSvgpreviousVertex(previous) {
        const previousId = getNodeId(previous);
        const currentId = getNodeId(current);
        document.querySelectorAll(`g._${previousId}_`).forEach(g => {
            if (g.getAttribute('class').includes(`_${currentId}_`)) {
                g.querySelector('path').setAttribute('stroke', 'lightgray');
            } else if (prepreviousId === undefined || !g.getAttribute('class').includes(`_${prepreviousId}_`)) {
                g.remove();
            }
        });
        const previousCircle = previous.querySelector('ellipse + ellipse');
        previousCircle.setAttribute('stroke', 'lightgray');
        previousCircle.setAttribute('fill', 'none');
        if (prepreviousId === undefined) {
            previousCircle.setAttribute('stroke-width', '3');
        }
        previous.querySelector('text').setAttribute('style', 'fill: lightgray;');
    }



    document.body.onclick = (event) => {
        const tmp = event.target.closest('svg > g > g.node');
        if (tmp && (possibleIds === undefined || possibleIds.includes(getNodeId(tmp)))) {
            play(tmp);
            possibleIds = nextss[getNodeId(current)];
        }
    }
});
    
