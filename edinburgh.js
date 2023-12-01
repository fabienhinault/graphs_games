"use strict";
document.addEventListener('DOMContentLoaded', function() {
    let last;
    let current;
    let possibleIds;
    document.body.onclick = (event) => {
        const tmp = event.target.closest('svg > g > g.node');
        if (tmp && (possibleIds === undefined || possibleIds.includes(tmp.id))) {
            last = current;
            current = tmp;
            let ellipse = current.querySelector('ellipse')
            ellipse.setAttribute('fill', 'gray');
            if (last !== undefined) {
                document.querySelectorAll(`g._${last.id}`).forEach(_ => _.remove());
                last.remove();
            }
            possibleIds = nextss[current.id];
        }
    }
});
    
