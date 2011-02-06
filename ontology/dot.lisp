;;;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; base: 10; -*-
;;;;;
;;;;; dot.lisp
;;;;;
;;;;;   Graphviz DOT ontology
;;;;; 
;;;;; Author:      Dan Lentz, Lentz Intergalactic Softworks, Thu Jan 27 15:22:57 2011
;;;;; Maintainer:  <danlentz@gmail.com>
;;;;;
;;;;; My face is new, my license is expired, and I'm under a doctor's care!!!!
;;;;;

(in-package :gx-user)

(defpackage :dot (:use)
  (:export
    :Graph :Cluster :Sub-Graph :Node :Record :Edge
    :arrowhead :arrowsize :arrowtail :bgcolor :color :constraint :decorate 
    :fillcolor :fixedsize :fontcolor :fontname :fontsize :from :headport :url
    :id :label :labeldistance :labelfloat :labelfontcolor :labelfontname
    :labeljust :labelloc :lhead :ltail :margin :minlen :nodesep :rank :to
    :ratio :samehead :sametail :shape :size :style :taillabel :tailport :width
    :dir :file-name  :headlabel :height :labelfontsize  :rankdir :ranksep)
  (:documentation "http://ebu.gs/ns/dot#"))

(set-uri-namedspace-from-pkg (find-package :dot))

;;;
;;; DOT Ontology
;;;

(defIndividual dot::Ontology (rdf:type owl:Ontology)
  (rdf:about "http://ebu.gs/ns/dot#")
  (rdfs:label (@ "DOT implementation for RDFS/OWL" "en"))
  (owl:versionInfo (@ "1.0.0 Initial Version"))
  (dc:creator "Dan Lentz"@en)
  (dc::title (@ "Graphviz DOT Vocabulary for RDFS/OWL" "en"))
  (rdfs:seeAlso "http://www.martin-loetzsh.de/DOTML")
  (rdfs:comment (@ "The DOT Markup Language adapted to OWL from the
  ever-handy DotML by Martin Loetzsh" "en")))


;;;
;;; DOT Classes (Nodes)
;;;

(defConcept dot:Graph (rdf:type owl:Class)
  (rdfs:label     "Graph"@en)
  (skos:definition "The root element of a 'dot' graph. Contains
  general layout attributes."@en))

(defConcept dot:Cluster (rdf:type owl:Class)
  (rdfs:label     "Cluster"@en)
  (skos:definition "A group of nodes and edges that are laid out
  separately and then integrated as a unit into its parent graph, with
  a bounding rectangle drawn about it. If the cluster has a label
  parameter, this label is displayed within the rectangle. Note also
  that there can be clusters within clusters."@en))

(defConcept dot:Sub-Graph (rdf:type owl:Class)
  (rdfs:label     "Subgraph"@en)
  (skos:definition "Combines nodes to groups with rank
  constraints. Different from clusters, another node which is not part
  of the group can be placed between the nodes of the group."@en))

(defConcept dot:Node (rdf:type owl:Class)
  (rdfs:label     "Node"@en)
  (skos:definition "A single node of the graph. If a node appears
  inside a record, it has only the both attributes id and label."@en))

(defConcept dot:Record (rdf:type owl:Class)
  (rdfs:label     "Record"@en)
  (skos:definition "A record of nodes. If a record appears inside a
  record, then it has no attributes."@en))

(defConcept dot:Edge (rdf:type owl:Class)
  (rdfs:label     "Edge"@en)
  (skos:definition "An edge between two nodes."@en))

;;;
;;; DOT Properties
;;;

(defProperty dot:arrowhead (rdf:type owl:DatatypeProperty)
  (rdfs:domain dot:Edge)  (rdfs:range xsd:string)
  (rdfs:comment "one of: 'normal', 'inv', 'dot', 'invdot', 'odot',
  'invodot', 'none', 'tee', 'empty', 'invempty', 'diamond',
  'odiamond', 'box', 'obox', 'open', 'crow', 'halfopen")
  (skos:definition "Style of arrowhead on the head of an edge."@en))

(defProperty dot:arrowsize (rdf:type owl:DatatypeProperty)
  (rdfs:domain dot:Edge) (rdfs:range xsd:float)
  (skos:definition "Multiplicative scale factor for arrowheads."@en))

(defProperty dot:arrowtail (rdf:type owl:DatatypeProperty)
  (rdfs:domain dot:Edge)   (rdfs:range xsd:string)
  (rdfs:comment "one of: 'normal', 'inv', 'dot', 'invdot', 'odot',
  'invodot', 'none', 'tee', 'empty', 'invempty', 'diamond',
  'odiamond', 'box', 'obox', 'open', 'crow', 'halfopen")
  (skos:definition "Style of arrowhead on the tail of an edge."@en))

(defProperty dot:bgcolor (rdf:type owl:DatatypeProperty)
  (rdfs:domain
    (owl:Class
      (owl:unionOf
        dot:Graph
        dot:Cluster)))
  (rdfs:range xsd:hexBinary)
  (skos:definition "When attached to the root graph, this color is
    used as the background for entire canvas. For a cluster, it is
    used as the initial background for the cluster. If a cluster has a
    filled style, the cluster's fillcolor will overlay the background
    color."@en))

(defProperty dot:color (rdf:type owl:DatatypeProperty)
  (rdfs:domain
    (owl:Class
      (owl:unionOf
        dot:Cluster
        dot:Node
        dot:Record
        dot:Edge)))
  (rdfs:range xsd:hexBinary)
  (skos:definition "Basic drawing color for graphics."@en))

(defProperty dot:constraint (rdf:type owl:DatatypeProperty)
  (rdfs:domain dot:Edge)  (rdfs:range xsd:boolean)
  (skos:definition "If false, the edge is not used in ranking the
    nodes. Normally, edges are used to place the nodes on ranks. In
    the second graph below, all nodes were placed on different
    ranks. In the first example, the edge b -> c does not add a
    constraint during rank assignment, so the only constraints are
    that a be above b and c."@en))

(defProperty dot:decorate (rdf:type owl:DatatypeProperty)
  (rdfs:domain dot:Edge)  (rdfs:range xsd:boolean)
  (skos:definition "If true, the edge label is attached to the edge
    by a 2-segment polyline, underlining the label, then going to the
    closest point of spline."@en))

(defProperty dot:dir (rdf:type owl:DatatypeProperty)
  (rdfs:domain dot:Edge)  (rdfs:range xsd:string)
  (skos:definition "Sets the edge type for drawing arrowheads (not
    for ranking purposes)."@en))

(defProperty dot:file-name (rdf:type owl:DatatypeProperty)
  (rdfs:domain dot:Graph)  (rdfs:range xsd:string)
  (skos:definition "A file name without extension for the SVG chart
    to be generated. If the DotML graph is embedded into a HTML page,
    then the path is relative to the page. File names have to be
    unique within a page."@en))

(defProperty dot:fillcolor (rdf:type owl:DatatypeProperty)
  (rdfs:range xsd:hexBinary)
  (rdfs:domain
    (owl:Class
      (owl:unionOf
        dot:Cluster
        dot:Node
        dot:Record)))
  (skos:definition "Color used to fill the background of a node,
    record or cluster.  For nodes and records, the attribute style has to
    be set to 'filled'. If style='filled' and fillcolor is not defined,
    color is used. For clusters, if color is not defined, bgcolor is
    used. If this is not defined, the default (#FFFFFF) is used, except
    for shape=point."@en))

(defProperty dot:fixedsize (rdf:type owl:DatatypeProperty)
  (rdfs:domain dot:Node)  (rdfs:range xsd:boolean)
  (skos:definition "If true, the node size set by height and width
    is kept fixed and not expanded to contain the text label. Note
    that fixedsize doesn't work for record nodes."@en))

(defProperty dot:fontcolor (rdf:type owl:DatatypeProperty)
  (rdfs:range xsd:hexBinary)
  (rdfs:domain
    (owl:Class
      (owl:unionOf
        dot:Graph
        dot:Cluster
        dot:Node
        dot:Edge
        dot:Record)))
  (skos:definition "The font color for object labels"@en))

(defProperty dot:fontname (rdf:type owl:DatatypeProperty)
  (rdfs:range xsd:string)
  (rdfs:domain
    (owl:Class
      (owl:unionOf
        dot:Graph
        dot:Cluster
        dot:Node
        dot:Edge
        dot:Record)))
  (skos:definition "The name of the used font. (System dependent)"@en))

(defProperty dot:fontsize (rdf:type owl:DatatypeProperty)
  (rdfs:domain
    (owl:Class
      (owl:unionOf
        dot:Graph
        dot:Cluster
        dot:Node
        dot:Edge
        dot:Record)))
  (rdfs:range xsd:float)
  (skos:definition "The font size for object labels."@en))

(defProperty dot:from (rdf:type owl:DatatypeProperty)
  (rdfs:domain dot:Edge)
  (skos:definition "The id of the node where an edge comes from."@en))

(defProperty dot:headport (rdf:type owl:DatatypeProperty)
  (rdfs:domain dot:Edge)
  (skos:definition "Indicates where on the head node to attach the
    head of an edge."@en))

(defProperty dot:headlabel (rdf:type owl:DatatypeProperty)
  (rdfs:domain dot:Edge) (rdfs:range xsd:string)
  (skos:definition "Text label to be placed near the head of an
    edge. Change the appearance of the head label with the attributes
    labelfontname, labelfontcolor and labelfontsize."@en))

(defProperty dot:height (rdf:type owl:DatatypeProperty)
  (rdfs:domain
    (owl:Class
      (owl:unionOf
        dot:Node
        dot:Record)))
  (rdfs:range xsd:float)
  (skos:definition "The height of a node, in inches. Note that if
    the text label does not fit into the desired node height, the node
    height is expanded to contain the text label. Use the attribute
    fixedsize to prevent the node from expanding."@en))

(defProperty dot:id (rdf:type owl:DatatypeProperty)
  (rdfs:domain
    (owl:Class
      (owl:unionOf
        dot:Node
        dot:Cluster)))
  (rdfs:range xsd:string)
  (skos:definition "A unique identifier for a node or a
    cluster. Node ids are referenced by the edge attributes from and to,
    cluster ids by ltail and lhead.  If for a node the attribute label is
    not defined, the value of the attribute is used."@en))

(defProperty dot:label (rdf:type owl:DatatypeProperty)
  (rdfs:domain
    (owl:Class
      (owl:unionOf
        dot:Node
        dot:Cluster
        dot:Graph
        dot:Edge)))
  (rdfs:range xsd:string)
  (skos:definition "Text label attached to objects. Different from
    ids labels can contain almost any special character, but not \". If a
    node does not have the attribute label, the value of the attribute id
    is used. If a node shall not have a label, label='' must be used.  The
    escape sequences '\n', '\l' and '\r' divide the label into lines,
    centered, left-justified and right-justified, respectively.  Change
    the appearance of the labels with the attributes fontname, fontcolor
    and fontsize."@en))

(defProperty dot:labeldistance (rdf:type owl:DatatypeProperty)
  (rdfs:domain dot:Edge) (rdfs:range xsd:float)
  (skos:definition "Multiplicative scaling factor adjusting the
    distance from the headlabel/ taillabel to the head/tail
    node."@en))

(defProperty dot:labelfloat (rdf:type owl:DatatypeProperty)
  (rdfs:domain dot:Edge) (rdfs:range xsd:boolean)
  (skos:definition "If true, allows edge labels to be less
    constrained in position. In particular, it may appear on top of
    other edges"@en))

(defProperty dot:labelfontcolor (rdf:type owl:DatatypeProperty)
  (rdfs:domain dot:Edge) (rdfs:range xsd:hexBinary)
  (skos:definition "The font color for the headlabel and the
    taillabel of an edge."@en))

(defProperty dot:labelfontname (rdf:type owl:DatatypeProperty)
  (rdfs:domain dot:Edge) (rdfs:range xsd:string)
  (skos:definition "The name of the used font for the headlabel and
    the taillabel of an edge. (System dependent)"@en))

(defProperty dot:labelfontsize (rdf:type owl:DatatypeProperty)
  (rdfs:domain dot:Edge) (rdfs:range xsd:float)
  (skos:definition "The font size for the headlabel and the
    taillabel of an edge."@en))

(defProperty dot:labeljust (rdf:type owl:DatatypeProperty)
  (rdfs:domain dot:Cluster) (rdfs:range xsd:string)
  (skos:definition "Justification for cluster labels. If the
    attribute is 'r', the label is right-justified within bounding
    rectangle; otherwise, left-justified."@en))

(defProperty dot:labelloc (rdf:type owl:DatatypeProperty)
  (rdfs:domain dot:Cluster) (rdfs:range xsd:string)
  (skos:definition "Top/bottom placement of cluster labels. If the
    attribute is 't', place label at the top; if the attribute is 'b',
    place label at the bottom"@en))

(defProperty dot:lhead (rdf:type owl:DatatypeProperty)
  (rdfs:domain dot:Edge) (rdfs:range xsd:string)
  (skos:definition "Logical head of an edge. The value must be the
    id of a cluster containing the real head. The edge is then clipped
    to the boundary of the cluster"@en))

(defProperty dot:ltail (rdf:type owl:DatatypeProperty)
  (rdfs:domain dot:Edge) (rdfs:range xsd:string)
  (skos:definition "Logical tail of an edge. The value must be the
    id of a cluster containing the real tail. The the edge is then
    clipped to the boundary of the cluster"@en))

(defProperty dot:margin (rdf:type owl:DatatypeProperty)
  (rdfs:domain dot:Graph) (rdfs:range xsd:float)
  (skos:definition "The x and y margins of the graph canvas, in inches."@en))

(defProperty dot:minlen (rdf:type owl:DatatypeProperty)
  (rdfs:domain dot:Edge) (rdfs:range xsd:positiveInteger)
  (skos:definition "Minimum edge length (rank difference between head and tail)."@en))

(defProperty dot:nodesep (rdf:type owl:DatatypeProperty)
  (rdfs:domain dot:Graph) (rdfs:range xsd:float)
  (skos:definition "Minimum space between two adjacent nodes in the
    same rank, in inches."@en))

(defProperty dot:rank (rdf:type owl:DatatypeProperty)
  (rdfs:domain dot:Sub-Graph) (rdfs:range xsd:string)
  (skos:definition "Rank constraints on the nodes in a sub-graph.
    If rank='same', all nodes are placed on the same rank. If
    rank='min', all nodes are placed on the minimum rank. If
    rank='source', all nodes are placed on the minimum rank, and the
    only nodes on the minimum rank belong to some sub-graph whose rank
    attribute is 'source' or 'min'. Analogous criteria hold for
    rank='max' and rank='sink'.  Note: the minimum rank is topmost or
    leftmost, and the maximum rank is bottommost or rightmost."@en))

(defProperty dot:rankdir (rdf:type owl:DatatypeProperty)
  (rdfs:domain dot:Graph) (rdfs:range xsd:string)
  (skos:definition "Sets direction of graph layout. If rankdir='LR',
    the graph is laid out from left to right, i.e., directed edges
    tend to go from left to right. By default, graphs are laid out
    from top to bottom ('TB')."@en))

(defProperty dot:ranksep (rdf:type owl:DatatypeProperty)
  (rdfs:domain dot:Graph) (rdfs:range xsd:float)
  (skos:definition "The gives desired rank separation, in
    inches. This is the minimum vertical distance between the bottom
    of the nodes in one rank and the tops of nodes in the next."@en))

(defProperty dot:ratio (rdf:type owl:DatatypeProperty)
  (rdfs:domain
    (owl:Class
      (owl:unionOf
        dot:Cluster
        dot:Node
        dot:Record
        dot:Edge)))
  (rdfs:range xsd:hexBinary)
  (skos:definition "Sets the aspect ratio (drawing height/drawing
    width) for the drawing. Note that this is adjusted before the size
    attribute constraints are enforced.  If ratio is numeric, it is
    taken as the desired aspect ratio. Then, if the actual aspect
    ratio is less than the desired ratio, the drawing height is scaled
    up to achieve the desired ratio; if the actual ratio is greater
    than that desired ratio, the drawing width is scaled up. If ratio
    = 'fill' and the size attribute is set, the drawing is scaled to
    achieve the aspect ratio implied by size. As size is set, when the
    drawing is later scaled to fit that rectangle, the resulting
    picture will fill the rectangle. If ratio = 'compress' and the
    size attribute is set, dot attempts to compress the initial layout
    to fit in the given size. This achieves a tighter packing of nodes
    but reduces the balance and symmetry."@en))

(defProperty dot:samehead (rdf:type owl:DatatypeProperty)
  (rdfs:domain dot:Edge) (rdfs:range xsd:string)
  (skos:definition "edges with the same head node and the same
    samehead attribute value are aimed at the same point on the
    head."@en))

(defProperty dot:sametail (rdf:type owl:DatatypeProperty)
  (rdfs:domain dot:Edge) (rdfs:range xsd:string)
  (skos:definition "edges with the same tail node and the same
    sametail attribute value are aimed at the same point on the
    tail."@en))

(defProperty dot:shape (rdf:type owl:DatatypeProperty)
  (rdfs:domain dot:Node) (rdfs:range xsd:string)
  (rdfs:comment "one of: box, circle, ellipse, point, egg, triangle, plaintext,
  diamond, trapezium, parallelogram, house, hexagon, octagon, doublecircle,
  doubleoctagon, invtriangle, invtrapezium, invhouse, Mdiamond, Msquare, Mcircle"@en)
  (skos:definition "The shape of a node. The appearance of a node is
   also affected by the node attributes fixedsize, fontname,
   fontsize, height, label, style and width."@en))

(defProperty dot:size (rdf:type owl:DatatypeProperty)
  (rdfs:domain dot:Graph) (rdfs:range xsd:float)
  (skos:definition "Maximum width and height of drawing, in
    inches. If defined and the drawing is too large, the drawing is
    uniformly scaled down so that it fits within the given size. Note
    that there is some interaction between the size and ratio
    attributes."@en))

(defProperty dot:style (rdf:type owl:DatatypeProperty)
  (rdfs:domain
    (owl:Class
      (owl:unionOf
        dot:Cluster dot:Node dot:Record dot:Edge)))
  (rdfs:range xsd:string)
  (rdfs:comment "one of: 'dashed', 'dotted', 'solid', 'invis' and 'bold' and 'filled'")
  (skos:definition "Defines the graphic style on an object. If the
    style is 'filled', the attribute fillcolor defines how to fill the
    object."@en))

(defProperty dot:taillabel (rdf:type owl:DatatypeProperty)
  (rdfs:domain dot:Edge) (rdfs:range xsd:string)
  (skos:definition "Text label to be placed near the tail of an
    edge. Change the appearance of the tail label with the attributes
    labelfontname, labelfontcolor and labelfontsize."@en))

(defProperty dot:tailport (rdf:type owl:DatatypeProperty)
  (rdfs:domain dot:Edge) (rdfs:range xsd:string)  
  (skos:definition "Indicates where on the tail node to attach the
    tail of an edge."@en))

(defProperty dot:to (rdf:type owl:DatatypeProperty)
  (rdfs:domain dot:Edge) (rdfs:range xsd:string)
  (skos:definition "The id of the node where an edge goes to"@en))

(defProperty dot:url (rdf:type owl:DatatypeProperty)
  (skos:definition "dereferencable link represented by the associated
    graph element"@en))

(defProperty dot:width (rdf:type owl:DatatypeProperty)
  (rdfs:domain
    (owl:Class
      (owl:unionOf
        dot:Node
        dot:Record)))
  (skos:definition "The width of a node, in inches. Note that if the
    text label does not fit into the desired node width, the node
    width is expanded to contain the text label. Use the attribute
    fixedsize to prevent the node from expanding."@en))




;;;;;
;; Local Variables:
;; indent-tabs: nil
;; outline-regexp: ";;[;]+"
;; End:
;;;;;
