use std::collections::{HashMap, HashSet , VecDeque};
use std::rc::Rc;
use std::cell::RefCell;
use std::hash::{Hash, Hasher};
use std::io;
use std::fs::File;
use std::io::BufRead;
use std::fs::OpenOptions;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::env;
use std::sync::{Mutex, OnceLock};

static CHARMAP: OnceLock<Mutex<HashMap<char, usize>>> = OnceLock::new();
static CHARCOUNTER: OnceLock<Mutex<usize>> = OnceLock::new();

// Initializing global variables
fn init_globals() {
    CHARMAP.set(Mutex::new(HashMap::new())).unwrap_or_else(|_| {
        panic!("Failed to initialize CHARMAP");
    });
    CHARCOUNTER.set(Mutex::new(0)).unwrap_or_else(|_| {
        panic!("Failed to initialize CHARCOUNTER");
    });
}

// Converting regular expression into readable regular expression
fn regexpression(s: &str) -> String {
    let mut words = Vec::new(); // Stack of words
    let mut opt = Vec::new(); // Stack of operators
    let mut oper = Vec::new(); // Stack for operands

    let mut str = String::new();
    let mut i = 0;

    while i < s.len() {
        let c = s.chars().nth(i).unwrap(); // s[i]

        if c == ',' {
            i += 1;
            continue;
        }
        if c == ' ' {
            i += 1;
            continue;
        } 
        if c == '(' {
            if words.last() == Some(&"symbol".to_string()) {
                i += 1;

                let charmap = CHARMAP.get().unwrap();
                let mut charmap = charmap.lock().unwrap(); // Safely obtain a mutable lock
                let mut charcounter = CHARCOUNTER.get().unwrap().lock().unwrap();
                
                let symbol = s.chars().nth(i).unwrap();
                
                // Mapping symbols to numbers
                if !charmap.contains_key(&symbol) {
                    charmap.insert(symbol, *charcounter);
                    *charcounter += 1;
                }

                let mut sym = String::new();
                while i < s.len() && s.chars().nth(i).unwrap() != ')' {
                    sym.push(s.chars().nth(i).unwrap());
                    i += 1;
                }
                oper.push(sym);

                i += 1; // Skip ')'
                
                words.pop();
                continue;
            }
            opt.push("(".to_string());  // if not 'symbol' then push "(" to opt stack 
        } else if c == ')' {
            let word = words.pop().unwrap();
            opt.pop();

            let mut temp = String::from("(");
            if word == "star" {  // star closure of a regular expression
                temp.push_str(&oper.pop().unwrap());
                temp.push_str("*)");
                oper.push(temp);
            } else {
                let st2 = oper.pop().unwrap();
                let st1 = oper.pop().unwrap();
                temp.push_str(&st1);
                if word == "concat" { // concatenation  of two regular expression
                    temp.push('.');
                } else {   // union of two regular expresssion
                    temp.push('+');
                }
                temp.push_str(&st2);
                temp.push(')');
                oper.push(temp);
            }
        } else {      // collecting word like concat , union , star etc
            str.push(c);
            if i + 1 < s.len() && s.chars().nth(i + 1).unwrap() == '(' {
                words.push(str.clone());
                str.clear();
            }
        }
        i += 1;
    }

    oper.pop().unwrap() 
}

// Define State
#[derive(Debug)]
struct State {
    name: usize,                                         // Unique identifier
    transitions: HashMap<char, Vec<Rc<RefCell<State>>>>, // Symbol-based transitions
    epsilon_transitions: Vec<Rc<RefCell<State>>>,        // Epsilon transitions
    is_accept: bool,                                     // Accept state flag
}

impl State {
    fn new(name: usize, is_accept: bool) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            name,
            transitions: HashMap::new(),
            epsilon_transitions: Vec::new(),
            is_accept,
        }))
    }
}

// Wrapper around Rc<RefCell<State>> for hashing and equality based on State.name
#[derive(Clone, Debug)]
struct StatePtr(Rc<RefCell<State>>);

impl Hash for StatePtr {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.borrow().name.hash(state);
    }
}

impl PartialEq for StatePtr {
    fn eq(&self, other: &Self) -> bool {
        self.0.borrow().name == other.0.borrow().name
    }
}

impl Eq for StatePtr {}

// Define NFA with Rc for shared ownership of states
#[derive(Debug)]
struct NFA {
    start: Rc<RefCell<State>>,              // Start state
    accept_states: Vec<Rc<RefCell<State>>>, // Accepting states
}

impl NFA {
    fn new(start: Rc<RefCell<State>>, accept_states: Vec<Rc<RefCell<State>>>) -> Self {
        Self { start, accept_states }
    }
}

// Create an NFA for a single symbol
fn create_nfa(symbol: char, name_counter: &mut usize) -> NFA {
    let start = State::new(*name_counter, false); // assigning a unique identifer
    *name_counter += 1;
    let finish = State::new(*name_counter, true);
    *name_counter += 1;

    start.borrow_mut()
        .transitions
        .insert(symbol, vec![finish.clone()]); // adding transistion from start state to finish state for this symbol 

    NFA::new(start, vec![finish])
}

// Union of two NFAs
fn union_nfa(nfa1: NFA, nfa2: NFA, name_counter: &mut usize) -> NFA {
    let start = State::new(*name_counter, false);
    *name_counter += 1;

    start.borrow_mut().epsilon_transitions.push(nfa1.start.clone()); // epsilon transition from start state to nfa1->start state
    start.borrow_mut().epsilon_transitions.push(nfa2.start.clone()); // epsilon transition from start state to nfa2->start state

    let mut accept_states = nfa1.accept_states;
    accept_states.extend(nfa2.accept_states);

    NFA::new(start, accept_states) // adding accept states of nfa1 and nfa2 to start state
}

// Start Closure of a NFA
fn star_nfa(mut nfa: NFA, name_counter: &mut usize) -> NFA {
    let start = State::new(*name_counter, true); // Create a new start state
    *name_counter += 1;

    // Add epsilon transitions from the new start state to the original start state
    start.borrow_mut().epsilon_transitions.push(nfa.start.clone());

    // Add epsilon transitions from each accept state back to the original start state
    for state in &mut nfa.accept_states {
        state.borrow_mut().epsilon_transitions.push(nfa.start.clone());
    }

    // new start state will also be a accept state along with accept states of original nfa
    let mut new_accept_states = vec![start.clone()]; // New start state is also an accepting state
    new_accept_states.extend(nfa.accept_states.clone()); // Add the original NFA's accept states

    // Return the new NFA with the updated states
    NFA::new(start, new_accept_states)
}

// Concatenate two NFAs
fn concat_nfa(mut nfa1: NFA, nfa2: NFA) -> NFA {
    // Link all accept states of nfa1 to the start state of nfa2 via epsilon transitions
    for state in &mut nfa1.accept_states {
        state.borrow_mut().epsilon_transitions.push(nfa2.start.clone());
        state.borrow_mut().is_accept = false; // They are no longer accepting states
    }

    // The new accept states are those of nfa2
    nfa1.accept_states = nfa2.accept_states;

    nfa1
}

// Function to convert a regex to an NFA
fn regexto_nfa(regex: &str, name_counter: &mut usize) -> NFA {
    let mut oper_stack: Vec<NFA> = Vec::new(); // Stack for NFAs
    let mut opt_stack: Vec<char> = Vec::new(); // Stack for operators

    for c in regex.chars() {
        match c {
            '(' => {
                opt_stack.push('('); // Push '(' onto the operator stack
            }
            ')' => {
                // Process until '('
                while let Some(op) = opt_stack.pop() {
                    if op == '(' {
                        break;
                    }
                    process_operator(op, &mut oper_stack, name_counter);
                }
            }
            '.' | '+' | '*' => {
                opt_stack.push(c); // Push operators onto the stack
            }
            _ => {
                // Create an NFA for the symbol
                let nfa = create_nfa(c, name_counter);
                oper_stack.push(nfa);
            }
        }
    }

    // Process remaining operators
    while let Some(op) = opt_stack.pop() {
        process_operator(op, &mut oper_stack, name_counter);
    }

    // The final NFA is on top of the stack
    oper_stack.pop().unwrap()
}

// Function to process operators (star, union, concatenation)
fn process_operator(op: char, oper_stack: &mut Vec<NFA>, name_counter: &mut usize) {
    match op {
        '*' => {
            let nfa = oper_stack.pop().unwrap(); // Pop the top NFA
            let star_nfa = star_nfa(nfa, name_counter); // Apply the Kleene star
            oper_stack.push(star_nfa);
        }
        '+' => {
            let nfa2 = oper_stack.pop().unwrap(); // Second NFA
            let nfa1 = oper_stack.pop().unwrap(); // First NFA
            let union_nfa = union_nfa(nfa1, nfa2, name_counter); // Union of two NFAs
            oper_stack.push(union_nfa);
        }
        '.' => {
            let nfa2 = oper_stack.pop().unwrap(); // Second NFA
            let nfa1 = oper_stack.pop().unwrap(); // First NFA
            let concat_nfa = concat_nfa(nfa1, nfa2); // Concatenation of two NFAs
            oper_stack.push(concat_nfa);
        }
        _ => panic!("Unsupported operator: {}", op),
    }
}

#[allow(dead_code)]
// Function to print the state and its transitions
fn print_state(state: &State, visited: &mut HashSet<usize>) {
    if visited.contains(&state.name) {
        return; // Avoid infinite loops due to epsilon transitions
    }
    visited.insert(state.name);

    println!("State {}{}", state.name, if state.is_accept { " (Accepting)" } else { "" });

    // Print symbol-based transitions
    for (symbol, states) in &state.transitions {
        for next_state in states {
            println!("  -- {} --> State {}", symbol, next_state.borrow().name);
        }
    }

    // Print epsilon transitions
    for next_state in &state.epsilon_transitions {
        println!("  -- epsilon --> State {}", next_state.borrow().name);
    }

    // Recursively print transitions for all connected states
    for states in state.transitions.values() {
        for next_state in states {
            print_state(&next_state.borrow(), visited);
        }
    }
    for next_state in &state.epsilon_transitions {
        print_state(&next_state.borrow(), visited);
    }
}

#[allow(dead_code)]
// Function to print the NFA's states and transitions
fn print_nfa(nfa: &NFA) {
    println!("NFA States and Transitions:");
    let mut visited = HashSet::new();
    print_state(&nfa.start.borrow(), &mut visited);
}

fn compute_epsilon_closure(state: Rc<RefCell<State>>) -> HashSet<StatePtr> {
    let mut closure = HashSet::new();
    let mut to_visit = vec![StatePtr(state.clone())];

    while let Some(current_ptr) = to_visit.pop() {
        let current_state = current_ptr.0.borrow();

        if closure.contains(&current_ptr) {
            continue; // Skip already visited states
        }

        closure.insert(current_ptr.clone()); // Add to the closure

        // Explore epsilon transitions
        for next_state in &current_state.epsilon_transitions {
            let next_ptr = StatePtr(next_state.clone());
            if !closure.contains(&next_ptr) {
                to_visit.push(next_ptr);
            }
        }
    }

    closure
}

fn nfa_transitions_table(
    nfa_transitions: &mut Vec<Vec<HashSet<StatePtr>>>, // Use `StatePtr` for the HashSet
    nfa: &NFA,
    charmap: &HashMap<char, usize>,
) {
    let mut visited = HashSet::new(); // Store visited states as `StatePtr`
    let mut to_visit = vec![StatePtr(nfa.start.clone())];

    while let Some(current_ptr) = to_visit.pop() {
        let current_state = current_ptr.0.borrow();

        if visited.contains(&current_ptr) {
            continue; // Skip already visited states
        }

        visited.insert(current_ptr.clone()); // Mark current state as visited

        // Compute epsilon closure of the current state
        let start_closure = compute_epsilon_closure(current_ptr.0.clone());

        // For each state in the start_closure, process its transitions
        for closure_ptr in start_closure {
            let closure_state = closure_ptr.0.borrow();

            for (&symbol, next_states) in &closure_state.transitions {
                let symbol_index = charmap.get(&symbol).unwrap(); // Get the index of the symbol from the charmap

                // Compute epsilon closure of destination states
                for next_state in next_states {
                    let destination_closure = compute_epsilon_closure(next_state.clone());

                    for dest_ptr in destination_closure {
                        nfa_transitions[current_state.name][*symbol_index].insert(dest_ptr.clone()); // adding states for which we can reach there from current state via symbol

                        if !visited.contains(&dest_ptr) {
                            to_visit.push(dest_ptr);
                        }
                    }
                }
            }
        }
    }
}

#[allow(dead_code)]
fn print_nfa_transitions_table(
    nfa_transitions: &Vec<Vec<HashSet<StatePtr>>>, // Use `StatePtr` for HashSet
    charmap: &HashMap<char, usize>,
) {
    for (i, state_transitions) in nfa_transitions.iter().enumerate() {
        println!("State: {}", i);

        for (j, transitions) in state_transitions.iter().enumerate() {
            // Find the corresponding character for the transition
            for (symbol, index) in charmap.iter() {
                if *index == j {
                    print!("Char: {}   :-->   ", symbol);
                    break;
                }
            }

            for state_ptr in transitions {
                print!("State {}  ", state_ptr.0.borrow().name);
            }
            println!(); // Newline after listing transitions
        }
    }
}

// DFAState structure
#[derive(Debug)]
struct DFAState {
    name: usize,
    state_subset: HashSet<StatePtr>,
    is_accept: bool,
    transitions: HashMap<char, DFAStatePtr>,
}

#[derive(Clone, Debug)]
struct DFAStatePtr {
    inner: Rc<RefCell<DFAState>>,
    name: usize, // Store the state's name for hashing and equality
}

impl DFAStatePtr {
    fn new(state: Rc<RefCell<DFAState>>) -> Self {
        let name = state.borrow().name; // Extract the name once
        Self { inner: state, name }
    }
}

impl Hash for DFAStatePtr {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state); // Use the pre-stored name for hashing
    }
}

impl PartialEq for DFAStatePtr {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name // Compare using the pre-stored name
    }
}

impl Eq for DFAStatePtr {}

// DFA representation
#[derive(Debug)]
struct DFA {
    start: DFAStatePtr,
    accept_states: Vec<DFAStatePtr>,
}

// Finding the set of states of NFA which will become start state for DFA 
fn start_state_dfa(nfa: &StatePtr) -> HashSet<StatePtr> {
    let mut start_state = HashSet::new();

    // Insert the initial state wrapped as `StatePtr`
    start_state.insert(nfa.clone());

    // Insert all epsilon transitions wrapped as `StatePtr`
    for epsilon_state in &nfa.0.borrow().epsilon_transitions {
        start_state.insert(StatePtr(epsilon_state.clone()));
    }

    start_state
}

// Wrapper around HashSet<StatePtr> to implement Hash and Eq
#[derive(Clone, Debug)]
struct StateSet(HashSet<StatePtr>);

impl Hash for StateSet {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // Compute hash based on the names of states in the set
        let mut state_names: Vec<_> = self.0.iter().map(|ptr| ptr.0.borrow().name).collect();
        state_names.sort_unstable(); // Ensure consistent ordering
        state_names.hash(state);
    }
}

impl PartialEq for StateSet {
    fn eq(&self, other: &Self) -> bool {
        // Compare based on the state names in the set
        let self_names: HashSet<_> = self.0.iter().map(|ptr| ptr.0.borrow().name).collect();
        let other_names: HashSet<_> = other.0.iter().map(|ptr| ptr.0.borrow().name).collect();
        self_names == other_names
    }
}

impl Eq for StateSet {}

// Function to convert NFA to DFA
fn nfa_to_dfa(
    nfa_transitions: &Vec<Vec<HashSet<StatePtr>>>,
    start_state: HashSet<StatePtr>,  // new start state for DFA 
    charmap: &HashMap<char, usize>, // character map
    dfa_state_counter: &mut usize, // for making unique ids for all dfa states
    dfa_state_map: &mut HashMap<usize, DFAStatePtr>,
) -> DFA {
    let dfa_start = DFAStatePtr::new(Rc::new(RefCell::new(DFAState {
        name: *dfa_state_counter,
        state_subset: start_state.clone(),
        is_accept: start_state.iter().any(|state| state.0.borrow().is_accept), // making it accept state if any nfa state is accept state
        transitions: HashMap::new(),
    })));
    
    *dfa_state_counter += 1; // incrementing the dfa state counter

    dfa_state_map.insert(dfa_start.inner.borrow().name, dfa_start.clone());  // for keeping track of dfa states

    let mut dfa = DFA {
        start: dfa_start.clone(),
        accept_states: if dfa_start.inner.borrow().is_accept {
            vec![dfa_start.clone()]
        } else {
            Vec::new()
        },
    };

    let mut unprocessed_states = VecDeque::new();  // queue storing states whose transition is not figured out
    unprocessed_states.push_back(dfa_start.clone());

    let mut subset_to_dfa_state: HashMap<StateSet, DFAStatePtr> = HashMap::new();  // keeping track of those set of states to their corresponding DFA state
    subset_to_dfa_state.insert(StateSet(start_state.clone()), dfa_start.clone());

    while let Some(current_dfa_state) = unprocessed_states.pop_front() {
        // Collect the current state's data first
        let current_state_subset: HashSet<StatePtr> =
            current_dfa_state.inner.borrow().state_subset.clone(); // Clone subset
    
        // Drop the immutable borrow of `current_dfa_state` here
        drop(current_dfa_state.inner.borrow());
    
        for (&symbol, &symbol_index) in charmap {
            let mut reachable_states = HashSet::new();
    
            // Compute reachable states for the current symbol
            for nfa_state in &current_state_subset { // taking each nfa state in that subset and adding those states which it can reach using that symbol
                for target in &nfa_transitions[nfa_state.0.borrow().name][symbol_index] {
                    reachable_states.insert(target.clone());
                }
            }
    
            let reachable_states_set = StateSet(reachable_states.clone());
    
            if let Some(existing_dfa_state) = subset_to_dfa_state.get(&reachable_states_set) { // if already marked this subset in map
                // Mutably borrow `current_dfa_state` to update its transitions
                current_dfa_state
                    .inner
                    .borrow_mut()
                    .transitions
                    .insert(symbol, existing_dfa_state.clone());
            } else {
                // Making DFA state for this newly found subset of states of nfa
                let is_accept = reachable_states.iter().any(|state| state.0.borrow().is_accept);
                let new_dfa_state = DFAStatePtr::new(Rc::new(RefCell::new(DFAState {
                    name: *dfa_state_counter,
                    state_subset: reachable_states.clone(),
                    is_accept,
                    transitions: HashMap::new(),
                })));
                
                *dfa_state_counter += 1;
    
                dfa_state_map.insert(new_dfa_state.inner.borrow().name, new_dfa_state.clone());  // adding new state of dfa to map
                subset_to_dfa_state.insert(reachable_states_set.clone(), new_dfa_state.clone()); // adding mapping from set of states to new dfa state
    
                if is_accept {
                    dfa.accept_states.push(new_dfa_state.clone()); // if new_dfa state is accept state then adding this entry to DFA
                }
    
                // Mutably borrow `current_dfa_state` to update its transitions
                current_dfa_state
                    .inner
                    .borrow_mut()
                    .transitions
                    .insert(symbol, new_dfa_state.clone());
    
                unprocessed_states.push_back(new_dfa_state);
            }
        }
    }    
    dfa
}

#[allow(dead_code)]
// Function to print the DFA
fn print_dfa(dfa: &DFA) {
    println!("DFA States and Transitions:");

    let mut visited = HashSet::new();
    let mut states_queue = VecDeque::new();
    states_queue.push_back(dfa.start.clone());

    while let Some(current_state) = states_queue.pop_front() {
        if visited.contains(&current_state.inner.borrow().name) {
            continue;
        }

        visited.insert(current_state.inner.borrow().name);

        let current_state_ref = current_state.inner.borrow();
        println!(
            "State {}{}:",
            current_state_ref.name,
            if current_state_ref.is_accept {
                " (Accepting)"
            } else {
                ""
            }
        );

        for (&symbol, next_state) in &current_state_ref.transitions {
            println!("  -- {} --> State {}", symbol, next_state.inner.borrow().name);
            if !visited.contains(&next_state.inner.borrow().name) {
                states_queue.push_back(next_state.clone());
            }
        }
    }
}

fn complement_dfa(dfa: &mut DFA) {
    let mut states_queue = VecDeque::new(); // Queue for BFS traversal
    let mut visited = HashSet::new(); // Set to track visited states

    // Start traversal from the start state
    states_queue.push_back(dfa.start.clone());
    visited.insert(dfa.start.clone());

    while let Some(current_state) = states_queue.pop_front() {
        let mut current_state_ref = current_state.inner.borrow_mut();

        // making accept as reject and reject as accept
        current_state_ref.is_accept = !current_state_ref.is_accept;

        // Enqueue unvisited states connected via transitions
        for next_state in current_state_ref.transitions.values() {
            if visited.insert(next_state.clone()) {
                states_queue.push_back(next_state.clone());
            }
        }
    }
}

// MiniDFAState structure
#[derive(Debug)]
struct MiniDFAState {
    name: usize,
    dfa_state_subset: HashSet<DFAStatePtr>,
    is_accept: bool,
    transitions: HashMap<char, MiniDFAStatePtr>,
}

impl MiniDFAState {
    fn new(name: usize) -> Self {
        MiniDFAState {
            name,
            dfa_state_subset: HashSet::new(),
            is_accept: false,
            transitions: HashMap::new(),
        }
    }
}

#[derive(Clone, Debug)]
struct MiniDFAStatePtr {
    inner: Rc<RefCell<MiniDFAState>>,
    name: usize, // Store the state's name for hashing and equality
}

impl MiniDFAStatePtr {
    fn new(state: Rc<RefCell<MiniDFAState>>) -> Self {
        let name = state.borrow().name; // Extract the name once
        Self { inner: state, name }
    }
}

impl Hash for MiniDFAStatePtr {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state); // Use the pre-stored name for hashing
    }
}

impl PartialEq for MiniDFAStatePtr {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name // Compare using the pre-stored name
    }
}

impl Eq for MiniDFAStatePtr {}

// MiniDFA structure
#[derive(Debug)]
struct MiniDFA {
    start: MiniDFAStatePtr,
    accept_states: Vec<MiniDFAStatePtr>,
}

// DSU initialization function
fn dsu_initialization(dfa_state_counter: usize) -> (Vec<usize>, Vec<usize>) {
    let parent = (0..dfa_state_counter).collect::<Vec<_>>();
    let rankdsu = vec![0; dfa_state_counter];
    (parent, rankdsu)
}

// DSU find function with path compression
fn find_by_rank(a: usize, parent: &mut Vec<usize>) -> usize {
    if parent[a] != a {
        parent[a] = find_by_rank(parent[a], parent);
    }
    parent[a]
}

// DSU union function with rank optimization
fn union_by_rank(a: usize, b: usize, parent: &mut Vec<usize>, rankdsu: &mut Vec<usize>) {
    let parent_a = find_by_rank(a, parent);
    let parent_b = find_by_rank(b, parent);

    if parent_a != parent_b {
        if rankdsu[parent_a] < rankdsu[parent_b] {
            parent[parent_a] = parent_b;
        } else if rankdsu[parent_a] > rankdsu[parent_b] {
            parent[parent_b] = parent_a;
        } else {
            parent[parent_a] = parent_b;
            rankdsu[parent_b] += 1;
        }
    }
}

// Minimizing the DFA
fn dfa_to_minidfa(
    dfa: &DFA,
    dfa_state_counter: usize,
    dfa_state_map: &HashMap<usize, DFAStatePtr>,
    charmap: &HashMap<char, usize>,
) -> MiniDFA {

    let mut state_status = vec![vec![false; dfa_state_counter]; dfa_state_counter]; // (ij)th cell representing ith and jth state

    // marking those pairs in which one is accepting while other is rejecting state
    for i in 1..dfa_state_counter {
        for j in 0..i {
            let state_i = dfa_state_map[&i].inner.borrow();
            let state_j = dfa_state_map[&j].inner.borrow();
            if state_i.is_accept != state_j.is_accept {
                state_status[i][j] = true;
            }
        }
    }

    // Checking all pairs
    // Marking that pair for which one state is reaching to some final state and other in not reaching some final state for atleast one symbol
    let mut changed = true;
    while changed {
        changed = false;
        for i in 1..dfa_state_counter {
            for j in 0..i {
                if !state_status[i][j] {
                    let state1 = dfa_state_map[&i].inner.borrow();
                    let state2 = dfa_state_map[&j].inner.borrow();
                    for (&symbol, _) in charmap {
                        let state1_dest = state1.transitions[&symbol].inner.borrow().name;
                        let state2_dest = state2.transitions[&symbol].inner.borrow().name;
                        if state_status[state1_dest][state2_dest] || state_status[state2_dest][state1_dest] {
                            state_status[i][j] = true;
                            changed = true;
                            break;
                        }
                    }
                }
            }
        }
    }

    // DSU initialization
    let (mut parent, mut rankdsu) = dsu_initialization(dfa_state_counter);

    // Doing Union of all equivalent states
    for i in 1..dfa_state_counter {
        for j in 0..i {
            if !state_status[i][j] {
                union_by_rank(i, j, &mut parent, &mut rankdsu);
            }
        }
    }

    // Group states by their representative
    let mut grouped_states: HashMap<usize, Vec<usize>> = HashMap::new();
    for i in 0..dfa_state_counter {
        let rep = find_by_rank(i, &mut parent);
        grouped_states.entry(rep).or_default().push(i);
    }

    // Create MiniDFA states
    let mut mini_state_map: HashMap<usize, MiniDFAStatePtr> = HashMap::new();
    let mut mini_dfa = MiniDFA {
        start: MiniDFAStatePtr::new(Rc::new(RefCell::new(MiniDFAState::new(0)))),
        accept_states: Vec::new(),
    };

    // Making new minidfa states for all grouped states
    let mut mini_state_counter = 0;
    for (&rep, states) in &grouped_states {
        let new_state = MiniDFAStatePtr::new(Rc::new(RefCell::new(MiniDFAState::new(mini_state_counter))));
        mini_state_counter += 1;

        let mut new_state_ref = new_state.inner.borrow_mut();
        for &state_index in states {
            let dfa_state = dfa_state_map[&state_index].inner.borrow();
            new_state_ref.dfa_state_subset.insert(dfa_state_map[&state_index].clone()); // adding all equivalent states to new state 
            if dfa_state.is_accept {
                new_state_ref.is_accept = true;
                mini_dfa.accept_states.push(new_state.clone()); // checking if new minidfa state will be accept state or not
            }
        }
        mini_state_map.insert(rep, new_state.clone());
    }

    // Set up transitions for MiniDFA states
    for (&rep, states) in &grouped_states {
        let current_state = mini_state_map[&rep].clone();
        for (&symbol, _) in charmap {
            let dfa_state = dfa_state_map[&states[0]].inner.borrow();
            let destination_state = dfa_state.transitions[&symbol].inner.borrow().name;
            let destination_rep = find_by_rank(destination_state, &mut parent);
            current_state.inner.borrow_mut().transitions.insert(symbol, mini_state_map[&destination_rep].clone());
        }
    }

    // Set the start state
    let start_rep = find_by_rank(dfa.start.inner.borrow().name, &mut parent);
    mini_dfa.start = mini_state_map[&start_rep].clone();

    mini_dfa
}

#[allow(dead_code)]
// Function to print the MiniDFA
fn print_minidfa(minidfa: &MiniDFA) {
    println!("Minimized DFA States and Transitions:");
    let mut states_queue = VecDeque::new();
    let mut visited = HashSet::new();

    states_queue.push_back(minidfa.start.clone());
    visited.insert(minidfa.start.clone());

    while let Some(current_state) = states_queue.pop_front() {
        let current_state_ref = current_state.inner.borrow();

        println!(
            "State {}{}:",
            current_state_ref.name,
            if current_state_ref.is_accept {
                " (Accepting)"
            } else {
                ""
            }
        );

        for (&symbol, next_state) in &current_state_ref.transitions {
            println!("  -- {} --> State {}", symbol, next_state.inner.borrow().name);
            if visited.insert(next_state.clone()) {
                states_queue.push_back(next_state.clone());
            }
        }
    }
}

fn regex_to_minidfa(regex: &str) -> MiniDFA {

    // Create the NFA from the regex
    let mut name_counter = 0;
    let nfa = regexto_nfa(&regex, &mut name_counter);

    // Access the character map (CHARMAP) safely
    let charmap_lock = CHARMAP.get().expect("CHARMAP is not initialized");
    let charmap = charmap_lock.lock().expect("Failed to acquire lock for CHARMAP");

    // Initialize the `nfa_transitions` table
    let char_count = charmap.len();
    let mut nfa_transitions: Vec<Vec<HashSet<StatePtr>>> =
        vec![vec![HashSet::new(); char_count]; name_counter];

    // Fill the `nfa_transitions` table
    nfa_transitions_table(&mut nfa_transitions, &nfa, &charmap);

    // Compute the start state of the DFA
    let start_state = start_state_dfa(&StatePtr(nfa.start.clone()));

    let mut dfa_state_counter = 0;
    let mut dfa_state_map: HashMap<usize, DFAStatePtr> = HashMap::new();

    // Convert NFA to DFA
    let mut dfa = nfa_to_dfa(
        &nfa_transitions,
        start_state.clone(),
        &charmap,
        &mut dfa_state_counter,
        &mut dfa_state_map,
    );

    // Complement the DFA
    complement_dfa(&mut dfa);

    // Minimize the DFA
    let mini_dfa = dfa_to_minidfa(&dfa, dfa_state_counter, &dfa_state_map, &charmap);
    mini_dfa
}

// Genrating NFA that accepts string only and rejecting all other strings
fn string_to_nfa(s: &str, string_name_counter: &mut usize) -> NFA {
    let start_state = State::new(*string_name_counter, false);
    *string_name_counter += 1;

    let mut current_state = start_state.clone();

    for (i, c) in s.chars().enumerate() {

        let charmap = CHARMAP.get().unwrap();
        let mut charmap = charmap.lock().unwrap(); // Safely obtain a mutable lock
        let mut charcounter = CHARCOUNTER.get().unwrap().lock().unwrap();

        if !charmap.contains_key(&c) {
            charmap.insert(c, *charcounter);
            *charcounter += 1;
        }

        let next_state = State::new(*string_name_counter, i == s.len() - 1); // Mark the last state as accept
        *string_name_counter += 1;

        current_state.borrow_mut()
            .transitions
            .entry(c)
            .or_insert_with(Vec::new)
            .push(next_state.clone());

        current_state = next_state;
    }

    NFA::new(start_state, vec![current_state])
}

fn string_to_minidfa(s: &str) -> MiniDFA {
    let mut string_name_counter = 0;

    // Convert string to NFA
    let nfa = string_to_nfa(s, &mut string_name_counter);

    // Access the character map (CHARMAP) safely
    let charmap_lock = CHARMAP.get().expect("CHARMAP is not initialized");
    let charmap = charmap_lock.lock().expect("Failed to acquire lock for CHARMAP");

    // Initialize NFA transitions table
    let char_count = charmap.len();
    let mut nfa_transitions: Vec<Vec<HashSet<StatePtr>>> =
        vec![vec![HashSet::new(); char_count]; string_name_counter];

    // Filling NFA transitions table
    nfa_transitions_table(&mut nfa_transitions, &nfa, &charmap);

    // Get start state for DFA
    let start_state = start_state_dfa(&StatePtr(nfa.start.clone()));

    let mut dfa_state_counter = 0;
    let mut dfa_state_map: HashMap<usize, DFAStatePtr> = HashMap::new();

    // Convert NFA to DFA
    let dfa = nfa_to_dfa(
        &nfa_transitions,
        start_state.clone(),
        &charmap,
        &mut dfa_state_counter,
        &mut dfa_state_map,
    );

    // Convert DFA to Minimized DFA
    let mini_dfa = dfa_to_minidfa(&dfa, dfa_state_counter, &dfa_state_map, &charmap);
    mini_dfa
}

// Doing Product Construction of Two DFAs
fn product_construction(dfa1: &MiniDFA, dfa2: &MiniDFA) -> MiniDFA {
    use std::collections::{HashMap, HashSet, VecDeque};

    // Map to track visited state pairs
    let mut state_map: HashMap<(usize, usize), MiniDFAStatePtr> = HashMap::new();

    // Helper function to create a new MiniDFA state
    let create_minidfa_state = |name1: usize, name2: usize, is_accept: bool| -> MiniDFAStatePtr {
        let unique_name = name1 * 1000 + name2; // Generate unique name
        let new_state = MiniDFAState {
            name: unique_name,
            dfa_state_subset: HashSet::new(),
            is_accept,
            transitions: HashMap::new(),
        };
        MiniDFAStatePtr::new(Rc::new(RefCell::new(new_state)))
    };

    // Queue for BFS traversal
    let mut to_process: VecDeque<(MiniDFAStatePtr, MiniDFAStatePtr)> = VecDeque::new();

    // Start state of the product DFA
    let is_start_accept = dfa1.start.inner.borrow().is_accept && dfa2.start.inner.borrow().is_accept;
    let start_state = create_minidfa_state(dfa1.start.name, dfa2.start.name, is_start_accept);

    // Map the initial state
    state_map.insert((dfa1.start.name, dfa2.start.name), start_state.clone());
    to_process.push_back((dfa1.start.clone(), dfa2.start.clone()));

    // Initialize the result DFA
    let mut result_dfa = MiniDFA {
        start: start_state.clone(),
        accept_states: Vec::new(),
    };

    // BFS traversal
    while let Some((state1, state2)) = to_process.pop_front() {
        let current_state = state_map.get(&(state1.name, state2.name)).unwrap().clone();

        // Process transitions
        for (&symbol, next1) in &state1.inner.borrow().transitions {
            if let Some(next2) = state2.inner.borrow().transitions.get(&symbol) {
                let next_pair = (next1.name, next2.name); // next_pair will be a state of result DFA by product construction

                // If the pair has not been visited, create a new MiniDFA state
                if !state_map.contains_key(&next_pair) {
                    let is_accept = next1.inner.borrow().is_accept && next2.inner.borrow().is_accept;
                    let new_state = create_minidfa_state(next1.name, next2.name, is_accept);

                    // Add to the map and queue
                    state_map.insert(next_pair, new_state.clone());
                    to_process.push_back((next1.clone(), next2.clone()));
                }

                // Add the transition to the current state's transitions
                current_state.inner.borrow_mut().transitions.insert(
                    symbol,
                    state_map.get(&next_pair).unwrap().clone(),
                );
            }
        }
    }

    // Collect accepting states
    for state in state_map.values() {
        if state.inner.borrow().is_accept {
            result_dfa.accept_states.push(state.clone());
        }
    }

    result_dfa
}

fn main() -> io::Result<()> {
    init_globals();
    
    // Get command-line arguments
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Error: Give the path of the *.in file");
        std::process::exit(1);
    }

    // Use args[1] as the input file path directly
    let input_file = &args[1];
    if !Path::new(input_file).exists() {
        eprintln!("Error: File does not exist at path {}", input_file);
        return Ok(());
    }

    // Open the file
    let file = File::open(&input_file)?;
    // Use a buffered reader to read lines
    let reader = io::BufReader::new(file);
    // Create an iterator for the file lines
    let mut lines = reader.lines();

    // Read and parse the first line as an integer
    let n = match lines.next() {
        Some(Ok(line)) => match line.trim().parse::<i32>() {
            Ok(num) => num,
            Err(_) => {
                eprintln!("First line is not a valid integer.");
                return Ok(());
            }
        },
        Some(Err(e)) => {
            eprintln!("Error reading the first line: {}", e);
            return Ok(());
        }
        None => {
            eprintln!("File is empty.");
            return Ok(());
        }
    };

    // Extract the directory of the input file
    let input_path = Path::new(&input_file);
    let parent_dir = input_path
        .parent()
        .unwrap_or_else(|| Path::new(".")); // Use current directory if no parent exists

    // Generate the output file path in the same folder
    let output_path: PathBuf = parent_dir.join("output.out");

    // Open the file in write mode (creates it if it doesn't exist)
    let mut w_file = OpenOptions::new()
        .write(true)
        .create(true)
        .truncate(true) // Clears the file if it already exists
        .open(output_path)?;

    let mut i = 0;
    while i < n {
        // Read the first line of the pair
        let exp = match lines.next() {
            Some(Ok(line)) => line,
            Some(Err(e)) => {
                eprintln!("Error reading line: {}", e);
                return Ok(());
            }
            None => {
                eprintln!("Unexpected end of file while reading expressions.");
                return Ok(());
            }
        };

        // Read the second line of the pair
        let s = match lines.next() {
            Some(Ok(line)) => line,
            Some(Err(e)) => {
                eprintln!("Error reading line: {}", e);
                return Ok(());
            }
            None => {
                eprintln!("Unexpected end of file while reading strings.");
                return Ok(());
            }
        };
        let regex = regexpression(&exp);
        let mini_dfa1 = string_to_minidfa(&s);
        let mini_dfa2 = regex_to_minidfa(&regex);
        let final_dfa: MiniDFA = product_construction(&mini_dfa1, &mini_dfa2);
        if final_dfa.accept_states.is_empty() {
            writeln!(w_file , "Yes").expect("Failed to write to output file");
        } else {
            writeln!(w_file , "No").expect("Failed to write to output file");
        }
        i += 1;
    }
    Ok(())
}
