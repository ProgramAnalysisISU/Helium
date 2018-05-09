#include "helium/utils/Graph.h"
#include "helium/parser/Visitor.h"
#include "helium/utils/Dot.h"
#include "helium/utils/AggGraph.h"
#include "helium/utils/FSUtils.h"

namespace hebigraph {
  template <typename T> std::string Graph<T>::getDotString(
                                                           std::function<std::string (T)> labelFunc
                                                           // std::string (*labelFunc)(T)
                                                           ) {
    // std::cout << "Getting dot string" << "\n";
    DotGraph dotgraph;
    std::map<T, int> IDs;
    int ID=0;
    {
      VertexIter begin,end;
      boost::tie(begin, end) = vertices(g);
      for (VertexIter it=begin;it!=end;++it) {
        Vertex v = *it;
        T t = Vertex2Node[v];
        std::string label = labelFunc(t);
        dotgraph.AddNode(std::to_string(ID), label);
        IDs[t]=ID;
        ID++;
      }
    }
    {
      EdgeIter begin,end;
      boost::tie(begin, end) = edges(g);
      for (EdgeIter it=begin;it!=end;++it) {
        Edge e = *it;
        Vertex src = source(e, g);
        Vertex dst = target(e, g);
        T srcT = Vertex2Node[src];
        T dstT = Vertex2Node[dst];
        std::string label = get(edge_label_t(), g, e);
        dotgraph.AddEdge(std::to_string(IDs[srcT]), std::to_string(IDs[dstT]), label);
      }
    }
    std::string dotstring = dotgraph.dump();
    // return visualize_dot_graph(dotstring);
    // std::cout << "Finished getting, returnning" << "\n";
    return dotstring;
  }

  template <typename T> std::string Graph<T>::getGgxString(std::function<std::string (T)> labelFunc) {
    AggGraph agg;
    std::map<T, int> IDs;
    int ID=0;
    {
      VertexIter begin,end;
      boost::tie(begin, end) = vertices(g);
      for (VertexIter it=begin;it!=end;++it) {
        Vertex v = *it;
        T t = Vertex2Node[v];
        std::string label = labelFunc(t);
        agg.addNode(std::to_string(ID), label);
        IDs[t]=ID;
        ID++;
      }
    }
    {
      EdgeIter begin,end;
      boost::tie(begin, end) = edges(g);
      for (EdgeIter it=begin;it!=end;++it) {
        Edge e = *it;
        Vertex src = source(e, g);
        Vertex dst = target(e, g);
        T srcT = Vertex2Node[src];
        T dstT = Vertex2Node[dst];
        std::string label = get(edge_label_t(), g, e);
        agg.addEdge(std::to_string(IDs[srcT]), std::to_string(IDs[dstT]), label);
      }
    }
    std::string aggstring = agg.dump();
    std::string dir = utils::create_tmp_dir();
    std::string agg_filename = dir + "/" + "out" + ".ggx";
    // utils::write_file(agg_filename, aggstring);
    // return agg_filename;
    return aggstring;
  }

  template <typename T> std::string Graph<T>::getGrsString(std::function<std::string (T)> labelFunc) {
    AggGraph agg;
    std::map<T, int> IDs;
    int ID=0;
    {
      VertexIter begin,end;
      boost::tie(begin, end) = vertices(g);
      for (VertexIter it=begin;it!=end;++it) {
        Vertex v = *it;
        T t = Vertex2Node[v];
        std::string label = labelFunc(t);
        agg.addNode(std::to_string(ID), label);
        IDs[t]=ID;
        ID++;
      }
    }
    {
      EdgeIter begin,end;
      boost::tie(begin, end) = edges(g);
      for (EdgeIter it=begin;it!=end;++it) {
        Edge e = *it;
        Vertex src = source(e, g);
        Vertex dst = target(e, g);
        T srcT = Vertex2Node[src];
        T dstT = Vertex2Node[dst];
        std::string label = get(edge_label_t(), g, e);
        agg.addEdge(std::to_string(IDs[srcT]), std::to_string(IDs[dstT]), label);
      }
    }
    // GRS
    std::string aggstring = agg.dump_grs();
    return aggstring;
  }
  

  template <typename T> void Graph<T>::merge(Graph<T> &rhs) {
    {
      VertexIter begin,end;
      boost::tie(begin, end) = vertices(rhs.g);
      for (VertexIter it=begin;it!=end;++it) {
        Vertex v = *it;
        T node = rhs.Vertex2Node[v];
        this->addNode(node);
      }
    }
    {
      EdgeIter begin,end;
      boost::tie(begin,end) = edges(rhs.g);
      for (EdgeIter it=begin;it!=end;++it) {
        Edge e = *it;
        Vertex src = source(e, rhs.g);
        Vertex dst = target(e, rhs.g);
        // Also I need to transfer the edge label
        std::string label = get(edge_label_t(), rhs.g, e);
        this->addEdge(rhs.Vertex2Node[src], rhs.Vertex2Node[dst], label);
      }
    }
  }


  /**
   * Modifying this to use manually defined entries and exits
   */
  // template <typename T> std::set<T> Graph<T>::getEntry() {
  //   std::set<T> ret;
  //   VertexIter begin,end;
  //   boost::tie(begin,end) = vertices(g);
  //   for (VertexIter it=begin;it!=end;++it) {
  //     Vertex v = *it;
  //     if (in_degree(v, g) == 0) ret.insert(Vertex2Node[v]);
  //   }
  //   return ret;
  // }
  // template <typename T> std::set<T> Graph<T>::getExit() {
  //   std::set<T> ret;
  //   VertexIter begin,end;
  //   boost::tie(begin,end) = vertices(g);
  //   for (VertexIter it=begin;it!=end;++it) {
  //     Vertex v = *it;
  //     if (out_degree(v, g) == 0) ret.insert(Vertex2Node[v]);
  //   }
  //   return ret;
  // }

  template <typename T> void Graph<T>::removeNodeGentle(T x, std::string label) {
    if (hasNode(x)) {
      EdgeIter begin,end;
      boost::tie(begin, end) = edges(g);
      std::set<Vertex> outset;
      std::set<Vertex> inset;
      for (EdgeIter it=begin;it!=end;++it) {
        Edge e = *it;
        if (Node2Vertex[x] == source(e, g)) {
          outset.insert(target(e, g));
        }
        if (Node2Vertex[x] == target(e, g)) {
          inset.insert(source(e, g));
        }
      }
      // connect
      for (Vertex in : inset) {
        for (Vertex out : outset) {
          // addEdge(in, out);
          Edge e = add_edge(in, out, g).first;
          put(edge_label_t(), g, e, label);
          // add_edge(in, out, g);
        }
      }
      // remove edge
      for (Vertex in : inset) {
        remove_edge(in, Node2Vertex[x], g);
      }
      for (Vertex out : outset) {
        remove_edge(Node2Vertex[x], out, g);
      }
      // remove node
      Vertex v = Node2Vertex[x];
      Vertex2Node.erase(v);
      Node2Vertex.erase(x);
      // CAUTION remove_vertex will invalidate some iterators, cause bug
      remove_vertex(v, g);
    }
  }

  template <typename T> void Graph<T>::removeCallsite(T x) {
    if (hasNode(x)) {
      EdgeIter begin,end;
      boost::tie(begin, end) = edges(g);
      std::set<Vertex> outset;
      std::set<Vertex> inset;
      std::set<Vertex> outset_inner;
      std::set<Vertex> inset_inner;
      for (EdgeIter it=begin;it!=end;++it) {
        Edge e = *it;
        std::string label = get(edge_label_t(), g, e);
        if (Node2Vertex[x] == source(e, g)) {
          if (label == "Call") {
            outset_inner.insert(target(e, g));
          } else {
            outset.insert(target(e, g));
          }
        }
        if (Node2Vertex[x] == target(e, g)) {
          if (label == "Return") {
            inset_inner.insert(source(e, g));
          } else {
            inset.insert(source(e, g));
          }
        }
      }
      // connect
      for (Vertex in : inset) {
        for (Vertex out : outset_inner) {
          // addEdge(in, out);
          // add_edge(in, out, g);
          Edge e = add_edge(in, out, g).first;
          put(edge_label_t(), g, e, "enter");
        }
      }
      for (Vertex in : inset_inner) {
        for (Vertex out : outset) {
          Edge e = add_edge(in, out, g).first;
          put(edge_label_t(), g, e, "leave");
          // add_edge(in, out, g);
        }
      }
      // remove edge
      for (Vertex in : inset) {
        remove_edge(in, Node2Vertex[x], g);
      }
      for (Vertex in : inset_inner) {
        remove_edge(in, Node2Vertex[x], g);
      }
      for (Vertex out : outset) {
        remove_edge(Node2Vertex[x], out, g);
      }
      for (Vertex out : outset_inner) {
        remove_edge(Node2Vertex[x], out, g);
      }
      // remove node
      Vertex v = Node2Vertex[x];
      Vertex2Node.erase(v);
      Node2Vertex.erase(x);
      // CAUTION remove_vertex will invalidate some iterators, cause bug
      remove_vertex(v, g);
    }
  }
  
  
  template class hebigraph::Graph<CFGNode*>;
}
