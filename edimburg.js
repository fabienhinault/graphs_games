"use strict";
document.addEventListener('DOMContentLoaded', function() {
    let last;
    let current;
    document.body.onclick = (event) => {
        const tmp = event.target.closest('svg > g > g.node');
        console.debug(tmp);
        if (tmp) {
            last = current;
            current = tmp;
            let ellipse = current.querySelector('ellipse')
            console.log(ellipse);
            ellipse.setAttribute('fill', 'gray');
            if (last !== undefined) {
                document.querySelectorAll(`g._${last.id}`).forEach(_ => _.remove());
                last.remove();
            }
        }
    }
});
    
