#ifndef GRAPH_H
#define GRAPH_H

#include <boost/utility.hpp>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/topological_sort.hpp>
#include <boost/graph/depth_first_search.hpp>
#include <boost/graph/dijkstra_shortest_paths.hpp>
#include <boost/graph/visitors.hpp>

// using namespace boost;


namespace hebigraph {

  using namespace boost;
  // using namespace std;
  // typedef pair<int,int> Edge;

  struct edge_label_t {
    typedef edge_property_tag kind;
  };
  typedef property<edge_label_t, std::string> EdgeLabelProperty;
  struct vertex_label_t {
    typedef vertex_property_tag kind;
  };
  typedef property<vertex_label_t, std::string> VertexLabelProperty;
  // typedef property<vertex_distance_t, std::string> VertexLabelProperty;
  // typedef std::map<vertex_desc, size_t> IndexMap;
  // IndexMap mapIndex;
  

  // this is super ... shit. To use listS instead of vecS, I have to
  // add this weired thing for vertex property
  typedef adjacency_list_traits<listS, listS, 
                                directedS>::vertex_descriptor vertex_descriptor;
  typedef  property<vertex_index_t, int, 
    property<vertex_name_t, char,
    property<vertex_distance_t, int,
             property<vertex_predecessor_t, vertex_descriptor> > > > VP;
  // typedef adjacency_list<listS, listS, bidirectionalS,
  //                        VertexLabelProperty, EdgeLabelProperty> GraphType;
  typedef adjacency_list<listS, listS, bidirectionalS,
                         VP, EdgeLabelProperty> GraphType;

  // typedef adjacency_list < listS, listS, directedS,
  //                          property<vertex_index_t, int, 
  //   property<vertex_name_t, char,
  //   property<vertex_distance_t, int,
  //            property<vertex_predecessor_t, vertex_descriptor> > > >, 
  //   property<edge_weight_t, int> > graph_t;

  typedef graph_traits<GraphType>::vertex_descriptor Vertex;
  typedef graph_traits<GraphType>::edge_descriptor Edge;
  typedef graph_traits<GraphType>::vertex_iterator VertexIter;
  typedef graph_traits<GraphType>::edge_iterator EdgeIter;


  struct cycle_detector : public dfs_visitor<>
  {
    cycle_detector(bool& has_cycle) 
      : m_has_cycle(has_cycle) { }

    template <class Edge, class Graph>
    void back_edge(Edge, Graph&) { m_has_cycle = true; }
  protected:
    bool& m_has_cycle;
  };

  
  template <typename T> class Graph {
  public:
    Graph() {}
    ~Graph() {}
    
    bool hasEdge(T x, T y) {
      return edge(Node2Vertex[x], Node2Vertex[y], g).second;
    }
    void addEdge(T x, T y, std::string label="") {
      if (hasEdge(x, y)) return;
      Edge e = add_edge(Node2Vertex[x], Node2Vertex[y], g).first;
      // add edge label
      put(edge_label_t(), g, e, label);
    }
    void removeEdge(T x, T y) {
      if (!hasEdge(x,y)) return;
      remove_edge(Node2Vertex[x], Node2Vertex[y], g);
    }
    void addEdge(std::set<T> fromset, std::set<T> toset, std::string label="") {
      for (T from : fromset) {
        for (T to : toset) {
          addEdge(from, to, label);
        }
      }
    }
    void addEdge(std::set<T> fromset, T to, std::string label="") {
      for (T from : fromset) {
        addEdge(from, to, label);
      }
    }
    void addEdge(T from, std::set<T> toset, std::string label="") {
      for (T to : toset) {
        addEdge(from, to, label);
      }
    }

    std::set<T> getPredecessor(T node) {
      std::set<T> ret;
      for (auto &m : Node2Vertex) {
        T from = m.first;
        if (hasEdge(from, node)) {
          ret.insert(from);
        }
      }
      return ret;
    }
    std::set<T> getSuccessor(T node) {
      std::set<T> ret;
      for (auto &m : Node2Vertex) {
        T to = m.first;
        if (hasEdge(node, to)) {
          ret.insert(to);
        }
      }
      return ret;
    }

    bool hasNode(T x) {
      return Node2Vertex.count(x) == 1;
    }
    void addNode(T x) {
      if (!hasNode(x)) {
        Vertex vertex = add_vertex(g);
        Node2Vertex[x] = vertex;
        Vertex2Node[vertex] = x;
      }
    }
    std::set<T> getAllNodes() {
      std::set<T> ret;
      for (auto &m : Node2Vertex) {
        ret.insert(m.first);
      }
      return ret;
    }
    void removeNode(T x) {
      if (Node2Vertex.count(x) == 1) {
        remove_vertex(Node2Vertex[x], g);
        Vertex2Node.erase(Node2Vertex[x]);
        Node2Vertex.erase(x);
      }
    }

    /**
     * Remove node gently
     * 1. get in and out
     * 2. connect in to out, omit all labels
     * 3. remove node and in and out edges
     */
    void removeNodeGentle(T x, std::string label="");
    void removeCallsite(T x);

    void removeOutEdge(T x) {
      if (Node2Vertex.count(x) == 1) {
        EdgeIter begin,end;
        boost::tie(begin, end) = edges(g);
        std::set<Vertex> toset;
        for (EdgeIter it=begin;it!=end;++it) {
          Edge e = *it;
          if (Node2Vertex[x] == source(e, g)) {
            // FIXME can i remove edge during iteration?
            // remove_edge(e, g);
            // T to = Vertex2Node[target(e, g)];
            toset.insert(target(e, g));
          }
        }
        // remvoe
        for (Vertex to : toset) {
          remove_edge(Node2Vertex[x], to, g);
        }
      }
    }

    void dump(std::ostream &os) {
      os << "Num of Vertices: " << num_vertices(g) << "\n";
      os << "Num of Edges: " << num_edges(g) << "\n";
      VertexIter begin,end;
      boost::tie(begin, end) = vertices(g);
      os << "Vertices:\n";
      for (VertexIter it=begin;it!=end;it++) {
        Vertex v = *it;
        os << v << ": ";
        // Vertex2Node[v]->dump(os);
        os << "\n";
      }
      os << "\n";
    }

    bool hasCycle() {
      bool has_cycle = false;
      cycle_detector vis(has_cycle);
      depth_first_search(g, visitor(vis));
      // cout << "The graph has a cycle? " << has_cycle << endl;
      return has_cycle;
    }

    std::vector<T> topoSort() {
      typedef std::list<Vertex> MakeOrder;
      MakeOrder::iterator i;
      MakeOrder make_order;

      topological_sort(g, std::front_inserter(make_order));
      // cout << "make ordering: ";
      // for (i = make_order.begin();
      //      i != make_order.end(); ++i) 
      //   cout << name[*i] << " ";
      
      std::vector<T> ret;
      for (Vertex v : make_order) {
        ret.push_back(Vertex2Node[v]);
      }
      return ret;
    }

    /**
     * visualize by exporting to graph
     */
    std::string getDotString(std::function<std::string (T)> labelFunc);
    std::string getGgxString(std::function<std::string (T)> labelFunc);
    std::string getGrsString(std::function<std::string (T)> labelFunc);

    void merge(Graph<T> &rhs);
  private:
    std::map<T,Vertex> Node2Vertex;
    std::map<Vertex,T> Vertex2Node;
    // adjacency_list<vecS, vecS, bidirectionalS> g;
    GraphType g;
  };

  template <typename T> std::string visualize(Graph<T> g);

  // /**
  //  * Connect graph -> graph
  //  */
  // friend template <typename T>
  // Graph<T> connect(Graph<T> from, std::set<T> fromnodes,
  //                  Graph<T> to, std::set<T> tonodes,
  //                  std::string label="");
  // /**
  //  * Connect graph -> node
  //  */
  // friend template <typename T>
  // Graph<T> connect(Graph<T> from, std::set<T> fromnodes,
  //                  std::set<T> to, std::string label="");
  // /**
  //  * Connect node -> graph
  //  */
  // friend template <typename T>
  // Graph<T> connect(std::set<T> from,
  //                  Graph<T> to, std::set<T> tonodes,
  //                  std::string label="");
}

#endif /* GRAPH_H */
