package vct.parsers.transform.systemctocol.engine;

import de.tub.pes.syscir.sc_model.*;
import de.tub.pes.syscir.sc_model.expressions.Expression;
import de.tub.pes.syscir.sc_model.expressions.FunctionCallExpression;
import de.tub.pes.syscir.sc_model.expressions.TimeUnitExpression;
import de.tub.pes.syscir.sc_model.variables.SCClassInstance;
import de.tub.pes.syscir.sc_model.variables.SCEvent;
import de.tub.pes.syscir.sc_model.variables.SCKnownType;
import de.tub.pes.syscir.sc_model.variables.SCTIMEUNIT;
import vct.col.ast.Class;
import vct.col.ast.InstanceField;
import vct.parsers.transform.systemctocol.exceptions.SystemCFormatException;
import vct.parsers.transform.systemctocol.exceptions.UnsupportedException;
import vct.parsers.transform.systemctocol.colmodel.*;
import vct.parsers.transform.systemctocol.util.Constants;
import vct.parsers.transform.systemctocol.util.Timing;
import vct.parsers.transform.systemctocol.util.OriGen;

/**
 * Main transformer class for the SCSystem. It handles the transformation of a given SystemC system into a COL model.
 * This transformer is the root of the transformer hierarchy tree.
 *
 * @param <T> IGNORED
 */
public class Transformer<T> {

	/**
	 * The SystemC Model to transform.
	 */
	private final SCSystem sc_system;

	/**
	 * COLSystem wrapping all classes of the COL system and all variable mappings.
	 */
	private final COLSystem<T> col_system;

	/**
	 * Default constructor initializing the attributes.
	 * 
	 * @param sc_system The System to transform
	 */
	public Transformer(SCSystem sc_system) {
		this.sc_system = sc_system;
		this.col_system = new COLSystem<>();
	}

	/**
	 * Creates the COL model based on the given SystemC model.
	 */
	public void create_col_model() {
		// Suitably configure time resolution for the SystemC system
		scan_time();
		// Calculate process and state classes to be translated
		scan_processes();
		// For each method in the SystemC system, find whether it waits or not
		// TODO: Do we want to try and reduce verification effort by not duplicating non-waiting methods?
		// Find methods that are used by multiple processes and should therefore be duplicated
		scan_function_usages();
		// Handle port/socket connections
		scan_port_connections();
		// Register enums to be translated to integer values
		scan_enums();
		// Create references to necessary Main class attributes
		create_minimal_main();
		// Translate system parameters
		translate_system_parameters();
		// Transform known types
		transform_known_types();
		// Transform shared event variables to event IDs
		handle_shared_events();
		// Transform all regular classes
		transform_classes();
		// Finish the Main class
		create_main_class();
	}

	/**
	 * Performs a preliminary scan of the SystemC system and sets the transformation's time resolution according to the
	 * lowest one used in the original.
	 */
	private void scan_time() {
		// Find all functions in the SystemC system
		java.util.List<SCFunction> functions = find_all_sc_functions();
		
		// Find all expressions in the SystemC system
		java.util.List<Expression> expressions = new java.util.ArrayList<>();
		for (SCFunction function : functions) {
			expressions.addAll(function.getAllExpressions());
		}
		
		// Find the minimum of all time expressions
		SCTIMEUNIT min = SCTIMEUNIT.SC_ZERO_TIME;
		for (Expression exp : expressions) {
			if (exp instanceof TimeUnitExpression time_unit) {
				if (time_unit.getTimeUnit().getExponent() < min.getExponent()) {
					min = time_unit.getTimeUnit();
				}
			}
		}
		
		// If physical time units were found, set the time resolution accordingly
		switch (min) {
			case SC_FS  -> Timing.SC_TIME_RESOLUTION = 1L;
			case SC_PS  -> Timing.SC_TIME_RESOLUTION = 1000L;
			case SC_NS  -> Timing.SC_TIME_RESOLUTION = 1000000L;
			case SC_US  -> Timing.SC_TIME_RESOLUTION = 1000000000L;
			case SC_MS  -> Timing.SC_TIME_RESOLUTION = 1000000000000L;
			case SC_SEC -> Timing.SC_TIME_RESOLUTION = 1000000000000000L;
			default -> {
				// Don't change if the only time resolution used is SC_ZERO_TIME
			}
		}
	}

	/**
	 * Iterates over all classes in the SystemC system (except SystemC-internal primitive channels, i.e. known types)
	 * and appropriately designs the encoding COL classes (i.e. process vs state classes) to encode each class. The
	 * design is only created for convenience and is transformed to COL later.
	 */
	private void scan_processes() {
		// Classes should be duplicated for each instance, not for each class
		for (SCClassInstance inst : sc_system.getInstances()) {
			// Known types are transformed separately
			if (!(inst instanceof SCKnownType)) {
				// Find member functions, constructor and member attributes of corresponding class
				java.util.List<SCFunction> member_functions = inst.getSCClass().getMemberFunctions();
				SCFunction constructor = inst.getSCClass().getConstructor();
				java.util.List<SCVariable> members = inst.getSCClass().getMembers();

				// Separate constructor and processes from regular member functions
				member_functions.remove(constructor);
				java.util.List<SCFunction> sc_processes = new java.util.ArrayList<>();
				for (SCProcess proc : inst.getSCClass().getProcesses()) {
					if (!proc.getType().equals(SCPROCESSTYPE.SCTHREAD)) {
						throw new UnsupportedException("Process " + proc + " is of unsupported type!");
					}
					member_functions.remove(proc.getFunction());
					sc_processes.add(proc.getFunction());
				}

				// Create classes
				switch (sc_processes.size()) {
					// No thread is declared -> create state class that contains all functionality
					case 0 -> {
						StateClass state_class = new StateClass(inst);
						state_class.set_constructor(constructor);
						state_class.add_attributes(members);
						state_class.add_methods(member_functions);
						col_system.add_state_class(inst, state_class);
					}
					// Exactly one thread is declared -> no need for state class, put all functionality in runnable class
					case 1 -> {
						ProcessClass proc = new ProcessClass(inst, sc_processes.get(0));
						proc.set_constructor(constructor);
						proc.add_attributes(members);
						proc.add_methods(member_functions);
						col_system.add_process(inst, proc);
					}
					// Multiple threads are declared -> one thread class for each thread and possibly a state class for shared functionality
					default -> {
						// Create state class if at least one member function or attribute is declared
						if (member_functions.size() > 0 || members.size() > 0) {
							StateClass state_class = new StateClass(inst);
							state_class.set_constructor(constructor);
							state_class.add_attributes(members);
							state_class.add_methods(member_functions);
							col_system.add_state_class(inst, state_class);
						}
						// For each thread, create its own thread function
						for (SCFunction sc_process : sc_processes) {
							ProcessClass proc = new ProcessClass(inst, sc_process);
							col_system.add_process(inst, proc);
						}
					}
				}
			}
		}
	}

	/**
	 * For each function in the SystemC system, finds all processes that use that function and registers them in the COL
	 * system context.
	 */
	private void scan_function_usages() {
		// For each process, find all functions it uses/might use
		for (ProcessClass proc : col_system.get_all_processes()) {
			// Start exploring with the run method
			SCFunction run_method = proc.get_generating_function();
			java.util.List<SCFunction> related_functions = new java.util.ArrayList<>();
			java.util.LinkedList<SCFunction> to_explore = new java.util.LinkedList<>();
			to_explore.add(run_method);

			while (!to_explore.isEmpty()) {
				SCFunction exploring = to_explore.removeFirst();

				// Add all newly found functions, if they haven't been visited before, to the list of visited functions
				for (SCFunction fun : explore_function_calls(exploring)) {
					if (!related_functions.contains(fun)) {
						related_functions.add(fun);
						if (!to_explore.contains(fun)) {
							to_explore.add(fun);
						}
					}
				}
			}

			// Add the process to the list of processes using the function in the global map
			for (SCFunction used_method : related_functions) {
				col_system.add_function_usage(used_method, proc);
			}
		}
	}

	/**
	 * Scans ports in the SystemC system for their respective connected channel instance.
	 */
	private void scan_port_connections() {
		// For each SystemC instance, iterate over all its port interfaces
		for (SCClassInstance sc_inst : sc_system.getInstances()) {
			for (SCConnectionInterface connection : sc_inst.getPortSocketInstances()) {
				if (connection instanceof SCPortInstance sc_port_inst) {
					// Get all connected SystemC-internal ("primitive") and user-defined ("hierarchical") channels
					SCPort sc_port = sc_port_inst.getPortSocket();
					java.util.List<SCKnownType> primitive = sc_port_inst.getChannels();
					java.util.List<SCClassInstance> hierarchical = sc_port_inst.getModuleInstances();

					// If there is a SystemC-internal channel, register it in the COL system context
					if (primitive.size() > 0) {
						col_system.add_primitive_port_connection(sc_inst, sc_port, primitive.get(0));
					}
					// If there is a user-defined channel, register it in the COL system context
					else if (hierarchical.size() > 0) {
						col_system.add_hierarchical_port_connection(sc_inst, sc_port, hierarchical.get(0));
					}
					else throw new SystemCFormatException("Port " + sc_port + " has no bound primitive or hierarchical channels!");
				}
			}
		}
	}

	/**
	 * Scans the SystemC system for enums and registers them with the COL system context.
	 */
	private void scan_enums() {
		for (SCEnumType enum_type : sc_system.getEnumTypes()) {
			col_system.add_enum(enum_type.getName());
		}
	}

	/**
	 * Creates the Main class's instance fields for the process and event state sequences. Does not yet instantiate the
	 * Main class itself.
	 */
	private void create_minimal_main() {
		col_system.set_process_state(new InstanceField<>(col_system.T_SEQ_INT, col_system.NO_FLAGS, OriGen.create("process_state")));
		col_system.set_event_state(new InstanceField<>(col_system.T_SEQ_INT, col_system.NO_FLAGS, OriGen.create("event_state")));
		col_system.set_primitive_channel_update(new InstanceField<>(col_system.T_SEQ_BOOL, col_system.NO_FLAGS, OriGen.create("primitive_channel_update")));
	}

	/**
	 * Finds and translates system parameters, and also registers them in the COL system for later use.
	 */
	private void translate_system_parameters() {
		// Transform explicitly defined system parameters
		for (SCVariable global_variable : sc_system.getGlobalVariables()) {
			// System parameters are declared as global const int fields		TODO: Discuss this encoding
			if (global_variable.isConst() && col_system.parse_type(global_variable.getType()).equals(col_system.T_INT)) {
				String var_name = "PARAM_" + global_variable.getName().toUpperCase();
				InstanceField<T> parameter_field = new InstanceField<>(col_system.T_INT, col_system.NO_FLAGS, OriGen.create(var_name));
				col_system.add_parameter(global_variable, parameter_field);
			}
		}

		// If the system uses FIFO queues, add a parameter for their size
		for (SCClassInstance sc_inst : sc_system.getInstances()) {
			// Check if any instance is a FIFO queue, add the parameter
			if (sc_inst.getType().equals(Constants.CLASS_FIFO_INT) || sc_inst.getType().equals(Constants.CLASS_FIFO_BOOL)) {
				InstanceField<T> buffer_size = new InstanceField<>(col_system.T_INT, col_system.NO_FLAGS, OriGen.create("BUFFER_SIZE"));
				col_system.set_fifo_size_parameter(buffer_size);
				break;
			}
		}
	}

	/**
	 * Transforms known types, i.e. SystemC-internal primitive channels, to their respective abstract encoding.
	 */
	private void transform_known_types() {
		for (SCClassInstance instance : sc_system.getInstances()) {
			if (instance instanceof SCKnownType inst) {
				KnownTypeTransformer<T> transformer = new KnownTypeTransformer<>(inst, col_system);
				transformer.transform();
			}
		}
	}

	/**
	 * Transforms explicit sc_event variables in the SystemC system to event IDs in the COL system.
	 */
	private void handle_shared_events() {
		for (SCClassInstance sc_inst : sc_system.getInstances()) {
			for (SCEvent sc_event : sc_inst.getSCClass().getEvents()) {
				int next_id = col_system.get_total_nr_events();
				col_system.add_shared_event(sc_inst, sc_event, next_id);
			}
		}
	}

	/**
	 * Transforms all classes of the SystemC model into COL classes.
	 */
	private void transform_classes() {
		for (SCClassInstance sc_inst : sc_system.getInstances()) {
			if (!(sc_inst instanceof SCKnownType)) {

				ClassTransformer<T> class_transformer = new ClassTransformer<>(col_system);

				StateClass state_cls = col_system.get_state_class(sc_inst);

				// Transform state class first, since process classes might call state class methods, but not the other way around
				if (state_cls != null) {
					Class<T> state_class = class_transformer.create_state_class(state_cls);
					col_system.add_global_declaration(state_class);
					col_system.add_col_class_translation(state_cls, state_class);
				}

				// Then transform all process classes
				for (ProcessClass process : col_system.get_processes(sc_inst)) {
					Class<T> process_class = class_transformer.create_process_class(process);
					col_system.add_global_declaration(process_class);
					col_system.add_col_class_translation(process, process_class);
				}
			}
		}
	}

	/**
	 * Generates the Main class. The main class contains an attribute for each instance in the encoded system, the
	 * global permission invariant, the permission invariant for the scheduler, the permission invariant for each
	 * primitive channel, and the scheduler method Main.main().
	 */
	private void create_main_class() {
		MainTransformer<T> main_transformer = new MainTransformer<>(sc_system, col_system);
		main_transformer.create_main_class();
	}

	// ============================================================================================================== //
	// ============================================= UTILITY FUNCTIONS ============================================== //
	// ============================================================================================================== //

	/**
	 * Finds all functions in the SystemC system.
	 *
	 * @return A list of all functions in the SystemC system
	 */
	private java.util.List<SCFunction> find_all_sc_functions() {
		java.util.List<SCFunction> result = new java.util.ArrayList<>(sc_system.getGlobalFunctions());
		for (SCClass sc_class : sc_system.getClasses()) {
			result.addAll(sc_class.getMemberFunctions());
		}
		return result;
	}

	/**
	 * Explores all expressions in a given function's body and returns a list of all function calls occurring inside
	 * that function body.
	 *
	 * @param f Function to scan
	 * @return A list of all functions called by <code>f</code>
	 */
	private java.util.List<SCFunction> explore_function_calls(SCFunction f) {
		java.util.List<SCFunction> result = new java.util.ArrayList<>();
		java.util.List<Expression> expressions = f.getAllExpressions();

		for (Expression expression : expressions) {
			if (expression instanceof FunctionCallExpression expr) {
				result.add(expr.getFunction());
			}
		}

		return result;
	}

	// ============================================================================================================== //
	// ============================================================================================================== //
	// ============================================================================================================== //

	public COLSystem<T> get_col_system() {
		return col_system;
	}
}
