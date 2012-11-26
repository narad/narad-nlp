package narad.generative
import scala.collection.mutable.HashMap
import scala.util.Random

class PitmanYor[T] {
//	val vt = new HashMap[V, T]
	val random = new Random
}

/*
	// n, total number of customers at tables with this label
	// m, number of tables with this label
	// nm, numbers of customers at table -> number of tables
  class T(n: Int, m: Int, nm: HashMap[Int, Int]) {
		
	}	
	
	bool empty() const { assert(m <= n); return n == 0; }
  
	def empty: Boolean = {
		assert(m <= n, "m > n")
		return n == 0
	}
	
	def insertNew() = {
		n += 1
		m += 1
		nm(1) += 1
	}
	
	override def toString = {
		val sb = new StringBuffer
		sb.append("n = " + n + ", ")
		sb.append("m = " + m + ", ")
		sb.append("label tables = (%s)".format(labelTables.mkString(", ")))
	}

	def erase(r: I) {
		n -= 1
		for (it <- nm) {
			if ((r =- (it.first * it.second)) <= 0) {
				val n1 = it.first
				if (it.second == 0) {
					nm.erase(it)
				}
				if (n1 == 0) {
					m -= 1
				}
				else {
					nm(n1) += 1
				}
				return n1
			}
			assert(r <= 0, "Should not reach this statement.")
			return 0
		}
	}
}
*/
/*
    U erase(I r) {
      --n;
      for (U_U::iterator it = n_m.begin(); it != n_m.end(); ++it) 
	if ((r -= it->first * it->second) <= 0) {
	  U n1 = it->first-1;  //!< new table size
	  if (--it->second == 0)
	    n_m.erase(it);
	  if (n1 == 0)
	    --m;
	  else
	    ++n_m[n1];
	  return n1;
	}
      assert(r <= 0);  // shouldn't ever get here
      return 0;
    }  // PYAdaptor::T::erase()
}

*/







/*
class PYAdaptor {

public:
  typedef typename Base::argument_type argument_type;
  typedef F result_type;

  Base& base;           //!< base distribution
  uniform01_type& u01;  //!< shared random number generator
  F a;                  //!< Pitman-Yor b parameter
  F b;                  //!< Pitman-Yor b parameter
  U m;                  //!< number of occupied tables
  U n;                  //!< number of customers in restaurant

  PYAdaptor(Base& base, uniform01_type& u01, F a=0, F b=1) : base(base), u01(u01), a(a), b(b), m(), n() { }

private:

  typedef argument_type V;
  typedef std::map<U,U> U_U;

  struct T { 
    U n;      //!< total number of customers at tables with this label
    U m;      //!< number of tables with this label
    U_U n_m;  //!< number of customers at table -> number of tables
    
    T() : n(), m() { } 
    
    //! insert_old() inserts a customer at a random old table
    //! using PY sampling distribution
    //
    void insert_old(F r, F a) { // when r is not positive, we have reached our table
      for (U_U::iterator it = n_m.begin(); it != n_m.end(); ++it) 
	if ((r -= it->second * (it->first - a)) <= 0) {
	  U n1 = it->first+1;  // new table size
	  if (--it->second == 0)
	    n_m.erase(it);
	  ++n_m[n1];           // add customer to new table
	  break;
	}
      assert(r <= 0);          // check that we actually updated a table
      ++n;                     // increment no of customers with this label
    }  // PYAdaptor::T::insert_old()

    //! insert_new() inserts a customer at a new table
    //
    void insert_new() {
      ++n;
      ++m;
      ++n_m[1];
    }  // PYAdaptor::T::insert_new()

    //! empty() is true if there are no customers left with this label
    //
    bool empty() const { assert(m <= n); return n == 0; }

    //! erase() removes a customer from a randomly chosen table,
    //!  returns number of customers left at table
    //
    U erase(I r) {
      --n;
      for (U_U::iterator it = n_m.begin(); it != n_m.end(); ++it) 
	if ((r -= it->first * it->second) <= 0) {
	  U n1 = it->first-1;  //!< new table size
	  if (--it->second == 0)
	    n_m.erase(it);
	  if (n1 == 0)
	    --m;
	  else
	    ++n_m[n1];
	  return n1;
	}
      assert(r <= 0);  // shouldn't ever get here
      return 0;
    }  // PYAdaptor::T::erase()

    //! sanity_check() checks that all of our numbers add up
    //
    bool sanity_check() const {
      assert(m > 0);
      assert(n > 0);
      assert(m <= n);
      U mm = 0, nn = 0;
      cforeach (U_U, it, n_m) {
	assert(n > 0);   // shouldn't have any empty tables
	assert(m > 0);  
	mm += it->second;
	nn += it->first * it->second;
      }
      bool sane_n = (n == nn);
      bool sane_m = (m == mm);
      assert(sane_n);
      assert(sane_m);
      return sane_n && sane_m;
    }  // PYAdaptor::T::sanity_check()

    std::ostream& print(std::ostream& os) const {
      return os << "(n=" << n << ", m=" << m << ", n_m=" << n_m << ")";
    }

  };  // PYAdaptor::T{}

  typedef tr1::unordered_map<V,T> V_T;

  V_T label_tables;

public:

  //! operator() returns the approximate probability for inserting v, with context
  //
  F operator() (const V& v) const {
    typename V_T::const_iterator tit = label_tables.find(v);
    F p_old = (tit == label_tables.end()) ? 0 : (tit->second.n - tit->second.m*a) / (n + b);
    F p_new = base(v) * (m*a + b) / (n + b);
    assert(p_new > 0);
    F sum_p = p_old + p_new;
    return sum_p;
  }  // PYAdaptor::operator()

  //! insert() adds a customer to a table, and returns its probability
  //
  F insert(const V& v) {
    ++n;    // one more customer
    typename V_T::iterator tit = label_tables.find(v);
    F p_old = (tit == label_tables.end()) ? 0 : (tit->second.n - tit->second.m*a);  //!< note: ignores (n - b) factor
    F p_new = base(v) * (m*a + b);
    F p = p_old + p_new;
    assert(p > 0);
    F r = p*u01();
    if (r <= p_old && tit != label_tables.end()) {  // insert at an old table
      assert(tit != label_tables.end());
      tit->second.insert_old(r, a);
    }
    else {                                 // insert customer at a new table
      T& t = (tit == label_tables.end()) ? label_tables[v] : tit->second;
      t.insert_new();
      ++m;    // one more table
      base.insert(v);
    }
    return p;
  }  // PYAdaptor::insert()

  //! erase() removes a customer at random from a restaurant
  //
  U erase(const V& v) {
    typename V_T::iterator tit = label_tables.find(v);
    assert(tit != label_tables.end());  // we should have tables with this label
    I r = (I) tit->second.n*u01();
    --n;  // one less customer
    if (tit->second.erase(r) == 0) {
      --m;
      base.erase(v);
      if (tit->second.empty())
	label_tables.erase(tit);
    }
    return n;
  }  // PYAdaptor::erase()

  //! empty() returns true if there are no customers in restaurant
  //
  bool empty() const { assert(m <= n); return n == 0; }

  //! clear() zeros out this adaptor.  You'll need to clear base() yourself.
  //
  void clear() {
    m = n = 0;
    label_tables.clear();
  }  // PYAdaptor::clear()

  //! logprob() returns the log probability of the table assignment in the
  //! adaptor.  You'll need to compute the base probability yourself
  //
  F logprob() const {
    F logp = 0;
    cforeach (typename V_T, it0, label_tables) {
      const U_U& n_m = it0->second.n_m;
      cforeach (U_U, it1, n_m)
	logp += it1->second * (lgamma(it1->first - a) - lgamma(1 - a));
    }
    if (a > 0)
      logp += m*log(a) + lgamma(m + b/a) - lgamma(b/a);
    else
      logp += m*log(b);
    logp -= lgamma(n + b) - lgamma(b);
    return logp;
  }  // PYAdaptor::logprob()

  //! print() prints the PY adaptor
  //
  std::ostream& print(std::ostream& os) const {
    os << "(n=" << n << ", m=" << m << ", label_tables=";
    char sep = '(';
    cforeach (typename V_T, it, label_tables) {
      os << sep << it->first << '=';
      it->second.print(os);
      sep = ',';
    }
    return os << "))";
  }

  //! sanity_check() ensures that all of the numbers in the adaptor
  //! add up
  //
  bool sanity_check() const {
    assert(m <= n);
    U nn = 0, mm = 0;
    bool sane_tables = true;
    cforeach (typename V_T, it, label_tables) {
      nn += it->second.n;
      mm += it->second.m;
      sane_tables = (sane_tables && it->second.sanity_check());
    }
    bool sane_n = (n == nn);
    bool sane_m = (m == mm);
    assert(sane_n);
    assert(sane_m);
    return sane_n && sane_m;
  }  // PYAdaptor::sanity_check()

};  // PYAdaptor{}


*/