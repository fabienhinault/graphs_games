
function createSvgPoint(svg, vertex) {
    let svgPoint = svg.createSVGPoint();
    svgPoint.x = vertex.x;
    svgPoint.y = vertex.y;
    return svgPoint;
}

function createPolygon(svg, svgVertex, cell) {
    const hes = cell.halfedges;
    const firstHalfEdge = hes[0];
    let polygon = document.createElementNS("http://www.w3.org/2000/svg", "polygon");
    polygon.points.appendItem(createSvgPoint(svg, firstHalfEdge.getStartpoint()));
    for (const halfEdge of hes) {
        polygon.points.appendItem(createSvgPoint(svg, halfEdge.getEndpoint()));
    }
    if (!cell.closeMe) {
        polygon.points.appendItem(createSvgPoint(svg, firstHalfEdge.getStartpoint()));
    }
    polygon.setAttribute('fill', 'white');
    svgVertex.insertBefore(polygon, svgVertex.firstChild); 
}

export function voronoize(svg, graphElement) {
    const box = graphElement.querySelector('polygon');
    const points = box.points;
    let voronoi = new Voronoi();
    const bbox = {xl: points[0].x, xr: points[2].x, yt: points[1].y, yb: points[0].y};
    const vertices = [...graphElement.querySelectorAll('g.node')];
    let sites = vertices.map(g => g.querySelector('ellipse')).map(e => { return { x: Number(e.getAttribute('cx')), y: Number(e.getAttribute('cy')) }; });
    const diagram = voronoi.compute(sites, bbox);
    for (let iVertex = 0; iVertex < vertices.length; iVertex++) {
        const voronoiId = sites[iVertex].voronoiId;
        const cell = diagram.cells[voronoiId];
        createPolygon(svg, vertices[iVertex], cell);
    }
}
