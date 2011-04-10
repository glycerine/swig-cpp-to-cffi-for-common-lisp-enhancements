/* -----------------------------------------------------------------------------
 * This file is part of SWIG, which is licensed as a whole under version 3 
 * (or any later version) of the GNU General Public License. Some additional
 * terms also apply to certain portions of SWIG. The full details of the SWIG
 * license and copyrights can be found in the LICENSE and COPYRIGHT files
 * included with the SWIG source code as distributed by the SWIG developers
 * and at http://www.swig.org/legal.html.
 *
 * cffi.cxx
 *
 * cffi language module for SWIG.
 * ----------------------------------------------------------------------------- */

char cvsroot_cffi_cxx[] = "$Id: cffi.cxx 12524 2011-03-09 21:42:38Z wsfulton $";


#include "swigmod.h"
#include "cparse.h"
#include <math.h>
#include <ctype.h>

#include <set>
#include <vector>
#include <algorithm>
#include <map>
#include <string>
#include <assert.h>
#include <unistd.h>

// DV: debug view macro
#ifndef DEBUGVIEW
#define DEBUGVIEW

// DV:
//
// to have DV statements executed while debugging
// with gdb, use "set enDV=1" at the (gdb) prompt
// use "set enDV=0" to turn them off again.
// DV statements, like asserts, are not compiled
// into release builds.

#ifndef DV
#ifdef NDEBUG
#define DV(x) ((void)0)

#else
extern int enDV; // off by default
#define DV(x) if (enDV) { x; }

#endif // NDEBUG
#endif // ndef DV

#endif // DEBUGVIEW

int enDV=0;


// Arme: and arity and method holder classes; caching state until we
// emit an overloaded method dispatcher after getting a
// defmacro call for the last overloaded method.
struct Arme {
  int     arity;
  Node*   method; // aka n
  String* swig_method_num;
  String* wrapname;

  String* sym_name2;
  String* args_placeholder;
  String* defcfun_call;
  String* args_call;
  String* args_names;
  String* sym_name_preswig;

  String* renamed_and_unscoped_method_name;
  String* renamed_class_name;
  String* class_dot_method;
  String* renamed_method;
};


struct LessThanArity {
  bool operator()(const Arme &a, const Arme &b) {
    return a.arity < b.arity;
  }
};

typedef std::set<Arme,LessThanArity>           AritySet;
typedef std::set<Arme,LessThanArity>::iterator AritySetIt;

typedef std::vector<Arme>           ArityVector;
typedef std::vector<Arme>::iterator ArityVectorIt;

// static variables : used to cache state between methods 
//                    in emit_overloaded_defgeneric_and_defun()

struct arity_vecset {
  AritySet     arityset;
  ArityVector  av;
};

  Arme         arme;


typedef std::map< std::string, arity_vecset*>  overload2vs;
typedef std::map< std::string, arity_vecset*>::iterator  o2vs_it;
overload2vs o2vs;


//#define CFFI_DEBUG
//#define CFFI_WRAP_DEBUG

static const char *usage = (char *) "\
CFFI Options (available with -cffi)\n\
     -generate-typedef - Use defctype to generate shortcuts according to the\n\
                         typedefs in the input.\n\
     -[no]cwrap        - Turn on or turn off generation of an intermediate C\n\
                         file when creating a C interface. By default this is\n\
                         only done for C++ code.\n\
     -[no]swig-lisp    - Turn on or off generation of code for helper lisp\n\
                         macro, functions, etc. which SWIG uses while\n\
                         generating wrappers. These macros, functions may still\n\
                         be used by generated wrapper code.\n\
     -structs-not-classes \n\
                       - By default we generate CLOS wrapper classes for C++ structs as well as \n\
                         C++ classes, so that methods defined inside of structs can \n\
                         be called. For backwards compatibility, this flag will go back\n\
			 to using defstruct only for structs, with no method wrappers.\n\
     -omit-readmacro-on-constants \n\
                       - skips the placement of the reader macro #. in front of constants\n\
                         This is needed for enum declarations that are self-referential.\n\
                         See notes in swig/Source/Modules/cffi.cxx.\n\
";

class CFFI:public Language {
public:
  String *f_cl;
  String *f_clhead;
  String *f_clwrap;
  bool CWrap;     // generate wrapper file for C code?  
  File *f_begin;
  File *f_runtime;
  File *f_cxx_header;
  File *f_cxx_wrapper;
  File *f_clos;

  String *module;
  virtual void main(int argc, char *argv[]);
  virtual int top(Node *n);
  virtual int functionWrapper(Node *n);
  virtual int variableWrapper(Node *n);
  virtual int constantWrapper(Node *n);
  //  virtual int classDeclaration(Node *n);
  virtual int enumDeclaration(Node *n);
  virtual int typedefHandler(Node *n);

  //C++ specific code
  virtual int constructorHandler(Node *n);
  virtual int destructorHandler(Node *n);
  virtual int memberfunctionHandler(Node *n);
  virtual int membervariableHandler(Node *n);
  virtual int classHandler(Node *n);
  virtual int staticmemberfunctionHandler(Node *n);

private:
  void emit_defun(Node *n, String *name);
  void emit_defmethod(Node *n);
  void emit_initialize_instance(Node *n);
  void emit_getter(Node *n);
  void emit_setter(Node *n);
  void emit_class(Node *n);
  void emit_struct_union(Node *n, bool un);
  void emit_export(Node *n, String *name);
  void emit_inline(Node *n, String *name);
  bool emit_overloaded_defgeneric_and_defun(Node *n,
					    String* sym_name2,
					    String* args_placeholder,
					    String* defcfun_call,
					    String* args_call,
					    String* args_names,
					    String* sym_name_preswig);

  int     get_arity_of_decl(Node *n);
  String* get_swig_method_number_suffix(Node* n);
  void    emit_lispfile_preamble(Node* f);
  static bool cffi_clos_preamble_emitted;
  void get_arg_details(Node* n, //in
		       String*& args_placeholder,
		       String*& args_names,
		       String*& args_call
		       );
  int count_sym_overloaded_methods(Node* n);

  String *lispy_name(char *name);
  String *lispify_name(Node *n, String *ty, const char *flag, bool kw = false);
  String *convert_literal(String *num_param, String *type, bool try_to_split = true);
  String *infix_to_prefix(String *val, char split_op, const String *op, String *type);
  String *strip_parens(String *string);
  String *trim(String *string);

  int generate_typedef_flag;
  int structs_as_classes_flag; 
  int omit_readmacro_on_constants_flag; 
  bool no_swig_lisp;
};

bool CFFI::cffi_clos_preamble_emitted = false;


void CFFI::main(int argc, char *argv[]) {
  int i;

  Preprocessor_define("SWIGCFFI 1", 0);
  SWIG_library_directory("cffi");
  SWIG_config_file("cffi.swg");
  generate_typedef_flag = 0;
  structs_as_classes_flag = 1;
  omit_readmacro_on_constants_flag = 0;
  no_swig_lisp = false;
  CWrap = false;
  for (i = 1; i < argc; i++) {
    if (!Strcmp(argv[i], "-help")) {
      Printf(stdout, "%s\n", usage);
    } else if (!strcmp(argv[i], "-cwrap")) {
      CWrap = true;
      Swig_mark_arg(i);
    } else if ((Strcmp(argv[i], "-generate-typedef") == 0)) {
      generate_typedef_flag = 1;
      Swig_mark_arg(i);
    } else if ((Strcmp(argv[i], "-structs-not-classes") == 0)) {
      structs_as_classes_flag = 0;
      Swig_mark_arg(i);
    } else if ((Strcmp(argv[i], "-omit-readmacro-on-constants") == 0)) {
      omit_readmacro_on_constants_flag = 1;
      Swig_mark_arg(i);
    } else if (!strcmp(argv[i], "-nocwrap")) {
      CWrap = false;
      Swig_mark_arg(i);
    } else if (!strcmp(argv[i], "-swig-lisp")) {
      no_swig_lisp = false;
      Swig_mark_arg(i);
    } else if (!strcmp(argv[i], "-noswig-lisp")) {
      no_swig_lisp = true;
      Swig_mark_arg(i);
    }

  }
  f_clhead = NewString("");
  f_clwrap = NewString("");
  f_cl = NewString("");

  allow_overloading();
}

void CFFI::emit_lispfile_preamble(Node* f) {

  Swig_banner_target_lang(f, ";;;");
  //  Swig_banner(f_clos);

  String* extra_use_pkgs = NewStringf(":ccl");

  Printf(f,
	 //	 "\n"
	 //	 "(defpackage :swig\n"
	 //	 "  (:use :common-lisp :cffi :ccl)\n"
	 //	 "  (:export #:*swig-identifier-converter* #:*swig-module-name*\n"
	 //	 "           #:*void* #:*swig-export-list*)\n"
	 //	 "  (:shadowing-import-from :cffi :defcallback))\n"
	 //	 "(in-package :swig)\n"
	 //	 "\n"
	 //	 "(eval-when (:compile-toplevel :load-toplevel :execute)\n"
	 //	 "  (defparameter *swig-identifier-converter* 'identifier-convert-null)\n"
	 //	 "  (defparameter *swig-module-name* :micro))\n"
	 "\n"
	 "(defpackage :%s\n"
	 "  (:shadowing-import-from :cffi :defcallback)\n"
	 "  (:use :common-lisp :cffi %s))\n"
	 //	 "  (:use :common-lisp :swig :cffi :ccl))\n"
	 "\n"
	 "(in-package :%s)\n"
	 "\n",
	 
	 module,
	 extra_use_pkgs,
	 module
	 );
  
}

int CFFI::top(Node *n) {
  File *f_null = NewString("");
  module = Getattr(n, "name");

  String *cxx_filename = Getattr(n, "outfile");
  String *lisp_filename = NewString("");

  Printf(lisp_filename, "%s%s.lisp", SWIG_output_directory(), module);

  File *f_lisp = NewFile(lisp_filename, "w", SWIG_output_files());
  if (!f_lisp) {
    FileErrorDisplay(lisp_filename);
    SWIG_exit(EXIT_FAILURE);
  }

  if (CPlusPlus || CWrap) {
    f_begin = NewFile(cxx_filename, "w", SWIG_output_files());
    if (!f_begin) {
      Close(f_lisp);
      Delete(f_lisp);
      Printf(stderr, "Unable to open %s for writing\n", cxx_filename);
      SWIG_exit(EXIT_FAILURE);
    }

    String *clos_filename = NewString("");
    Printf(clos_filename, "%s%s-clos.lisp", SWIG_output_directory(), module);
    f_clos = NewFile(clos_filename, "w", SWIG_output_files());
    if (!f_clos) {
      Close(f_lisp);
      Delete(f_lisp);
      Printf(stderr, "Unable to open %s for writing\n", cxx_filename);
      SWIG_exit(EXIT_FAILURE);
    }
  } else {
    f_begin = NewString("");
    f_clos = NewString("");
  }

  // f_clos good, now do clos_preamble
  emit_lispfile_preamble(f_clos);

  f_runtime = NewString("");
  f_cxx_header = f_runtime;
  f_cxx_wrapper = NewString("");

  Swig_register_filebyname("header", f_cxx_header);
  Swig_register_filebyname("wrapper", f_cxx_wrapper);
  Swig_register_filebyname("begin", f_begin);
  Swig_register_filebyname("runtime", f_runtime);
  Swig_register_filebyname("lisphead", f_clhead);
  if (!no_swig_lisp)
    Swig_register_filebyname("swiglisp", f_cl);
  else
    Swig_register_filebyname("swiglisp", f_null);

  Swig_banner(f_begin);

  Printf(f_runtime, "\n");
  Printf(f_runtime, "#define SWIGCFFI\n");
  Printf(f_runtime, "\n");

  emit_lispfile_preamble(f_lisp);

  Language::top(n);
  Printf(f_lisp, "%s\n", f_clhead);
  Printf(f_lisp, "%s\n", f_cl);
  Printf(f_lisp, "%s\n", f_clwrap);

  Close(f_lisp);
  Delete(f_lisp);   // Deletes the handle, not the file
  Delete(f_cl);
  Delete(f_clhead);
  Delete(f_clwrap);
  Dump(f_runtime, f_begin);
  Close(f_begin);
  Delete(f_runtime);
  Delete(f_begin);
  Delete(f_cxx_wrapper);
  Delete(f_null);

  return SWIG_OK;
}

int CFFI::classHandler(Node *n) {
#ifdef CFFI_DEBUG
  Printf(stderr, "class %s::%s\n", "some namespace",  //current_namespace,
   Getattr(n, "sym:name"));
#endif
  String *name = Getattr(n, "sym:name");
  String *kind = Getattr(n, "kind");

  // Handle structs the same as classes if structs_as_classes_flag is true.
  // This handles the common case in C++ wherein classes with all public
  // members and methods are simply declared as structs.
  if (    (Strcmp(kind, "class")  == 0)
      || ((Strcmp(kind, "struct") == 0) && structs_as_classes_flag)) {
               emit_class(n);
	       Language::classHandler(n);

  } else if (Strcmp(kind, "struct") == 0) {
    emit_struct_union(n, false);
    return SWIG_OK;
  } else if (Strcmp(kind, "union") == 0) {
    emit_struct_union(n, true);
    return SWIG_OK;
  } else {
    Printf(stderr, "Don't know how to deal with %s kind of class yet.\n", kind);
    Printf(stderr, " (name: %s)\n", name);
    SWIG_exit(EXIT_FAILURE);
    return SWIG_OK;
  }

  return SWIG_OK;
}

int CFFI::constructorHandler(Node *n) {
#ifdef CFFI_DEBUG
  Printf(stderr, "constructor %s\n", Getattr(n, "name"));
  Printf(stderr, "constructor %s\n and %s and %s", Getattr(n, "kind"), Getattr(n, "sym:name"), Getattr(n, "allegrocl:old-sym:name"));
#endif
  Setattr(n, "cffi:constructorfunction", "1");
  // Let SWIG generate a global forwarding function.
  return Language::constructorHandler(n);
}

int CFFI::destructorHandler(Node *n) {
#ifdef CFFI_DEBUG
  Printf(stderr, "destructor %s\n", Getattr(n, "name"));
#endif

  // Let SWIG generate a global forwarding function.
  return Language::destructorHandler(n);
}

// return the arity, or number of formal parameters to a method, for the n's declaration decl.
//    return -1 on error (no such attribute "decl").
//
int CFFI::get_arity_of_decl(Node *n) {

  /*
    examples of the different arity classes we might find:
            | decl         - "f()."       <- class 0   ; the distance between the "(" and ")" is 1.

            | decl         - "f(int)."    <- class 1   ; note the distance between "(" and ")" is > 1
            | decl         - "f(double)." <- class 1
            | decl         - "f(string)." <- class 1

            | decl         - "f(int,double)."     <- class 2 ; note how many commas between "(" and ")"
            | decl         - "f(int,int)."        <- class 2

	    etc.
   note that there is no reason that these need be a contiguous set of integers,
   so we need to track any gaps too.

  */
  String* decl = Getattr(n,"decl");
  if (0==decl) return -1;

  String* lparen = Strstr(decl,"(");
  if (0==lparen) return -1;

  String* rparen = Strstr(decl,")");
  if (0==rparen) return -1;

  String* zero_arity_paren = Strstr(decl,"()");
  if (zero_arity_paren) return 0;

  char* lpar = Char(lparen);
  //char* rpar = Char(rparen);

  int   comma_count = 0;
  char* start = lpar + 1;
  char* found = 0;

  while (1) {
    found = strstr(start,",");
    if (!found) break;
    ++comma_count;
    start = found+1;
  }

  // static methods don't have a this pointer.
  Node* storage = Getattr(n,"storage");
  if (storage && 0==Strcmp(storage,"static")) {
    return comma_count;
  }

  return comma_count + 1;
}


String* CFFI::get_swig_method_number_suffix(Node* n) {

  String* method_suffix = 0; // return this
  
  String* s = Getattr(n, "sym:name");
  if (!s) return 0;

  char* sym_name = Char(lispify_name(n, s, "'function"));
  if (!sym_name) return 0;

  char* match = strstr(sym_name,"__SWIG");
  if (match) {
    method_suffix = NewStringf(match + sizeof("__SWIG_")-1); // get just the number at the end;
  } else {
    method_suffix = NewStringf("");
  }
  
  return method_suffix;
}

// CFFI::emit_overloaded_defgeneric_and_defun
//    has to operate in one of two modes: either save the
//    signature until the last, or dump them all now that
//    we are on the last. The save mode is further refined
//    by either being the start of a new set (discard all
//    old, buffered methods) or a continuation

// returns true if we printed or cached for later printing, or is already handled; false otherwise if defmethod needs to print it.
bool CFFI::emit_overloaded_defgeneric_and_defun(Node *n,
						String* sym_name2,
						String* args_placeholder,
						String* defcfun_call,
						String* args_call,
						String* args_names,
						String* sym_name_preswig) {

    // guarantee that we only emit the manual dispatching code
    // and method declarations once and only once.
    if (Getattr(n,"cffi:emit_overloaded_defgeneric")) {
      DV(Printf(stdout,"^^^^^^^^^ : cffi:defgeneric_and_defun_already_done was already set for %s, aborting early.\n",sym_name2));
      return true; 
    } else {
      // set it so we will know next time through
      Setattr(n,"cffi:emit_overloaded_defgeneric","1");
    }

    String* overloaded = Getattr(n,"sym:overloaded");
    if (!overloaded) return false;

  // debug:
    DV( {
  printf("n:\n");
  if (n)                 Swig_print(n,enDV);
  printf("sym_name2:\n");
  if (sym_name2)         Swig_print(sym_name2,enDV);
  printf("defcfun_call:\n");
  if (defcfun_call)      Swig_print(defcfun_call,enDV);
  printf("args_call:\n");
  if (args_call)         Swig_print(args_call,enDV);
  printf("args_names:\n");
  if (args_names)        Swig_print(args_names,enDV);
  printf("sym_name_preswig:\n");
  if (sym_name_preswig)  Swig_print(sym_name_preswig,enDV);
      } );

  // count the number of different arity classes. Lisp methods can distinguish 
  //   methods with different types, but we have to manually dispatch the C++
  //   methods that overload on different numbers of arguments (arity).

    arity_vecset* av = 0;

    // sym_name_preswig is FileName_compare
    // sym_name_preswig is compare, for the static overload.

    String* renamed = Char(Getattr(n,"memberfunctionHandler:sym:name")); // compare for regular non-static.

    if (renamed == 0) {
      renamed = Char(Getattr(n,"sym:name"));
    }

    o2vs_it vit = o2vs.find(Char(renamed));
    DV(printf("dump of os2vs after looking for '%s'\n", Char(renamed)));
    DV(for (o2vs_it jit = o2vs.begin(); jit != o2vs.end(); ++jit) { printf("%s\n", jit->first.c_str()); } );

    if(vit == o2vs.end()) {
      av = new arity_vecset;
      o2vs[Char(renamed)] = av;      
    } else {
      av = vit->second;
    }

    AritySet&    defgen_arityset = av->arityset;
    ArityVector& defgen_av       = av->av; 

    // save the node's details (which disappear) so we can emit a full method plus arity
    // dispatch function later.
     arme.arity  = get_arity_of_decl(n);     
     arme.method           = Copy(n);
     arme.swig_method_num  = Copy(get_swig_method_number_suffix(n));
     arme.wrapname         = Copy(Getattr(n,"wrap:name"));

     arme.sym_name2        = Copy(sym_name2);
     arme.args_placeholder = Copy(args_placeholder);
     arme.defcfun_call     = Copy(defcfun_call);
     arme.args_call        = Copy(args_call);
     arme.args_names       = Copy(args_names);

     // the original sym_name_preswig wasn't consist across static 
     // and non-static methods, so we re-create it consistently below
     // once we have the two necessary components.
     //      no good:   arme.sym_name_preswig = Copy(sym_name_preswig);

     arme.renamed_and_unscoped_method_name = Copy(Getattr(arme.method,"memberfunctionHandler:sym:name"));
     arme.renamed_class_name               = Copy(Getattr(Getattr(arme.method,"parentNode"),"classDeclaration:name"));
     arme.renamed_method   = NewStringf("%s",renamed);
     arme.class_dot_method = NewStringf("%s.%s",arme.renamed_class_name, renamed);

     // good across static and non-static overloaded methods:
     arme.sym_name_preswig = NewStringf("%s_%s",arme.renamed_class_name, arme.renamed_method);
     sym_name_preswig = arme.sym_name_preswig; // we use it later, stay consistent.

     defgen_av.push_back(arme);
     defgen_arityset.insert(arme);

     DV(Printf(stdout,"arity seen was: %d  for  %s   :  %s  :  %s\n", arme.arity, Getattr(n,"sym:name"), Getattr(n,"decl"),Getattr(n,"sym:overname")));

     if (vit == o2vs.end()) return true; // it was new

     // not new, have we got all of them?
     if ((int)defgen_av.size() < count_sym_overloaded_methods(n)) return true; // no, come back later

     // yes, we've seen them all
     assert((int)defgen_av.size() == count_sym_overloaded_methods(n));
     DV(printf("issuing all %d generic methods + manual dispatcher now\n", (int)defgen_av.size()));
     assert(defgen_av.size() !=0);
     std::sort(defgen_av.begin(), defgen_av.end(), LessThanArity());

  // INVAR: defgen_arityset and defgen_av are loaded and ready.

  assert(defgen_av.size() !=0);
  ArityVectorIt env = defgen_av.end();
  ArityVectorIt it = defgen_av.begin();
  int i = 0;

  // (defgeneric f_1 (s x)

  for (; it != env; ++i) {
    // only advance it at the bottom of this loop - advance it manaully by num_this_arity

    // determine in advance how many we need to loop over, just to keep things simple
    int num_this_arity = 0;
    int cur_arity = it->arity;

    ArityVectorIt kit = it;
    for( ; kit != env; ++kit) {
      if (kit->arity == cur_arity) {
	++num_this_arity;
      } else {
	break;
      }
    }
    DV(printf("found %d in arity class %d\n",num_this_arity, cur_arity));
    
    String *cur_args_placeholder = NewStringf("");
    String *cur_args_names = NewStringf("");
    String *cur_args_call = NewStringf("");
    
    // fills in the last 3 args as side effects
    get_arg_details(it->method, cur_args_placeholder, cur_args_names, cur_args_call);

       DV( { printf("debug: cur_args_placeholder:\n");
	     Swig_print(cur_args_placeholder);
	     printf("debug: cur_args_names:\n");
	     Swig_print(cur_args_names);
	     printf("debug: cur_args_call:\n");
	     Swig_print(cur_args_call);
	       });

    Printf(f_clos,"\n(cl:defgeneric  %%%s_%d (%s)",sym_name_preswig, cur_arity, cur_args_names);

    DV(Printf(stdout,"\n(cl:defgeneric  %%%s_%d (%s)\n",sym_name_preswig, cur_arity, cur_args_names));

    // reset kit to the begininig of our class
    kit = it;
    for (int j = 0; j < num_this_arity; ++j, ++kit) {

      cur_args_placeholder = NewStringf("");
      cur_args_names = NewStringf("");
      cur_args_call = NewStringf("");

      get_arg_details(kit->method, cur_args_placeholder, cur_args_names, cur_args_call);

      // debug
      DV( {
      printf("debug: cur_args_placeholder:\n");
      Swig_print(cur_args_placeholder);
      printf("debug: cur_args_names:\n");
      Swig_print(cur_args_names);
      printf("debug: cur_args_call:\n");
      Swig_print(cur_args_call);
	});

      String* cur_defcfun_call = lispify_name(it->method, Getattr(kit->method, "sym:name"), "'function");
      Printf(f_clos,"\n (:method (%s)   (%s%s  %s))",cur_args_placeholder, sym_name_preswig, Getattr(kit->method,"sym:overname") , cur_args_call);


      // debug:
      DV(Printf(stdout,"\n (:method (%s)   (%s %s))\n",cur_args_placeholder, cur_defcfun_call, cur_args_call));
    }
    Printf(f_clos,")\n\n");

    it += num_this_arity;
  } // end it iterating over defgen_av


  DV(Swig_print(arme.method,enDV));

  Printf(f_clos,
	 "(declaim (inline %s))\n\n"
	 "(defun %s (&rest args)\n"
	 " \"arity dispatch for %s::%s in C/C++\"\n"
	 " (ecase (length args)\n",
	 arme.class_dot_method,
	 arme.class_dot_method,
	 arme.renamed_class_name,
	 arme.renamed_method
	 );
  
  AritySetIt en = defgen_arityset.end();
  i = 0;
  for (AritySetIt it = defgen_arityset.begin(); it != en; ++it, ++i) {
    
    arme = *it; // local copy. easy to reference and debug/inspect
    Printf(f_clos,"   (%d (apply #'%%%s_%d  args))\n", arme.arity + 1, arme.sym_name_preswig, arme.arity );
  }
  Printf(f_clos,"))\n\n"); 
  
  Printf(f_clos,"(cl:export '%s)\n\n",arme.class_dot_method);

  // loop it over the arity classes
       DV(Printf(stdout, "debug args:\n"
		 "sym_name2        = '%s'\n"
		 "args_placeholder = '%s'\n"
		 "defcfun_call     = '%s'\n"
		 "args_call        = '%s'\n"
		 "args_names       = '%s'\n"
		 "\n\n",
		 sym_name2, // DObject_compare
		 args_placeholder, // (self DObject) (obj DObject)
		 defcfun_call, // DObject_compare
		 args_call, // " (ff-pointer self) (ff-pointer obj)"
		 args_names));


       return true;
} // end emit_overloaded_defgeneric_and_defun()



void CFFI::get_arg_details(Node* n, //in
			   String*& args_placeholder,
			   String*& args_names,
			   String*& args_call
			   ) {

    ParmList *pl = Getattr(n, "parms");

    DV(printf("parms:\n"));
    DV(Swig_print(pl));

    int argnum = 0;
    Node *parent = getCurrentClass();
    bool first = 0;
  
    for (Parm *p = pl; p; p = nextSibling(p), argnum++) {
      String *argname = Getattr(p, "name");
      String *ffitype = Swig_typemap_lookup("lispclass", p, "", 0);

      DV( {
      printf("debug parms...here is p:\n");
      Swig_print(p);
      printf("debug parms...here is argname:\n");
      Swig_print(argname);
      printf("debug parms...here is ffitype:\n");
      Swig_print(ffitype);
	});

      int tempargname = 0;

      if(!first)
	first = true;
      else {
	Printf(args_placeholder, " ");
	Printf(args_names, " "); 
      }

      DV( {
	  printf("args_placeholer:\n");
	  Swig_print(args_placeholder);
	});

      DV( {printf("args_names:\n");
	  Swig_print(args_names); });

      if (!argname) {
	argname = NewStringf("arg%d", argnum);
	tempargname = 1;
      } else if (Strcmp(argname, "t") == 0 || Strcmp(argname, "T") == 0) {
	argname = NewStringf("t-arg%d", argnum);
	tempargname = 1;
      }
      if (Len(ffitype) > 0) {
	Printf(args_placeholder, "(%s %s)", argname, ffitype);
	Printf(args_names, " %s", argname);
      } else {
	Printf(args_placeholder, "%s", argname);
	Printf(args_names, "%s", argname);      
      }
    
      if (ffitype && Strcmp(ffitype, lispify_name(parent, lispy_name(Char(Getattr(parent, "sym:name"))), "'classname")) == 0)
	Printf(args_call, " (ff-pointer %s)", argname);
      else
	Printf(args_call, " %s", argname);

      Delete(ffitype);

      if (tempargname) {
	Delete(argname);
      }
    } // end pl loop


    DV({
      printf("debug: ---- at the end of get_arg_details ---\n");
      printf("debug: n:\n");
      Swig_print_node(n);
      printf("debug: args_placeholder:\n");
      Swig_print(args_placeholder);
      printf("debug: args_names:\n");
      Swig_print(args_names);
      printf("debug: args_call:\n");
      Swig_print(args_call);
      });


} // end CFFI::get_arg_details

int CFFI::count_sym_overloaded_methods(Node* n) {
    Node *overloaded = Getattr(n, "sym:overloaded");
    if (0==overloaded) return 0;

    Node* symnext = Getattr(n, "sym:nextSibling");
    Node* symprev = Getattr(n, "sym:previousSibling");    

    Node* symprev_last = symprev;
    Node* symnext_last = symnext;

    while (symnext) {
      symnext_last = symnext;
      symnext      = Getattr(symnext, "sym:nextSibling");
    }
    symnext = symnext_last;

    while (symprev) {
      symprev_last = symprev;
      symprev      = Getattr(symprev, "sym:previousSibling");
    }
    symprev = symprev_last;

    // count forward
    int prev_fwd_count = 0;
    if (symprev) {
      while(symprev) {
	prev_fwd_count++;
	symprev = Getattr(symprev, "sym:nextSibling");
      }
    }

    // count backward
    int next_bwd_count = 0;
    if (symnext) {
      while(symnext) {
	next_bwd_count++;
	symnext = Getattr(symnext, "sym:previousSibling");
      }
    }

    return std::max(prev_fwd_count,next_bwd_count);
}

void CFFI::emit_defmethod(Node *n) {

  DV( {
      printf("emit_defmethod called ================================================\n");
      Swig_print(n,2);
      printf("============================= end emit_defmethod dump ================\n");
    });

  Node *overloaded = Getattr(n, "sym:overloaded");

  String *args_placeholder = NewStringf("");
  String *args_names = NewStringf("");
  String *args_call = NewStringf("");

  get_arg_details(n, args_placeholder, args_names, args_call);

  String *method_name = Getattr(n, "name");
  int x = Replace(method_name, "operator ", "", DOH_REPLACE_FIRST); //  

  if (x == 1) {
    Printf(f_clos, "(cl:shadow \"%s\")\n", method_name);
  }

  // have to make a new string here, or else we'll mofiy "sym:name" permanantly during
  // the Replaceall -- definitely not what we want!
  String* sym_name2 = NewStringf("%s",Char(lispify_name(n, Getattr(n, "sym:name"), "'function")));
  String* sym_name_preswig = NewStringf("%s",sym_name2);
  //Swig_print(sym_name2);

  DV( {
  if (overloaded) {
    int ovcount = count_sym_overloaded_methods(n);
    printf("%s  has ovcount = %d\n",Char(sym_name_preswig),ovcount);
  }});

  // locate and define the __SWIG_1 or __SWIG_2 or whatever number we find here.
  char* sym_name2_startat = Char(sym_name2);
  char* swig_method_numstring = strstr(sym_name2_startat, "__SWIG_");

  String* SwigMethodNumber = 0;
  if (swig_method_numstring && strlen(swig_method_numstring)) {
    SwigMethodNumber = NewStringf("%s",swig_method_numstring); // preserve it for later

    int len = swig_method_numstring - sym_name2_startat;
    char* buf = new char[len+1];
    strncpy(buf,sym_name2_startat,len);
    buf[len]='\0';
    //Delete(sym_name_preswig);
    sym_name_preswig = NewStringf("%s",buf);
    //delete [] buf;

    DV( {
	printf("sym_name_preswig is: '%s'   which was from buf: '%s'\n",Char(sym_name_preswig),buf);
	Swig_print(sym_name_preswig);
      });
  } else {
    SwigMethodNumber = NewStringf("");
    swig_method_numstring = 0;
  }


  Replaceall(sym_name2,NewStringf("__SWIG_"),NewStringf("_"));

  
  DV( {printf("debug ======== sym_name2:\n");
      Swig_print(sym_name2); });

  String* defcfun_call = lispify_name(n, Getattr(n, "sym:name"), "'function");

  DV( {printf("debug ======== n:\n");
      Swig_print(n); });

  // call for everybody; it's harmless if not needed.
  if (overloaded) {
    emit_overloaded_defgeneric_and_defun(n,
					 sym_name2,
					 args_placeholder,
					 defcfun_call,
					 args_call,
					 args_names,
					 sym_name_preswig);
      return;
  }

  // INVAR: n is not an overloaded C++ method.


  String* renamed_and_unscoped_method_name = Getattr(n,"memberfunctionHandler:sym:name");
  String* renamed_class_name               = Getattr(Getattr(n,"parentNode"),"classDeclaration:name");
  String* class_dot_method = NewStringf("%s.%s", renamed_class_name, renamed_and_unscoped_method_name);

  /* make it easy to figure out which methods are being called, and avoid conflicts between C++ over-ridden methods */
  Printf(f_clos, "(cl:defmethod %s (%s)\n  (%s%s))\n\n",
         class_dot_method,
	 args_placeholder,
	 defcfun_call,
	 args_call);

  Setattr(n, "cffi:memberfunction", "1"); // what is the point of this--do we need to do it in emit_overloaded_defgeneric_and_defun as well?

} // end emit_defmethod


/* ----------------------------------------------------------------------
 * CFFI::staticmemberfunctionHandler()
 * ---------------------------------------------------------------------- */

int CFFI::staticmemberfunctionHandler(Node *n) {

  DV(printf("CFFI::staticmemberfunctionHandler(Node *n) called\n"));

  emit_defmethod(n);

  Setattr(n, "cffi:staticmemberfunctionHandler", "1");
  // Let SWIG generate a global forwarding function.
  return Language::staticmemberfunctionHandler(n);
}

void CFFI::emit_initialize_instance(Node *n) {
  String *args_placeholder = NewStringf("");
  String *args_call = NewStringf("");

  ParmList *pl = Getattr(n, "parms");
  int argnum = 0;
  Node *parent = getCurrentClass();

  for (Parm *p = pl; p; p = nextSibling(p), argnum++) {
    String *argname = Getattr(p, "name");
    String *ffitype = Swig_typemap_lookup("lispclass", p, "", 0);

    int tempargname = 0;
    if (!argname) {
      argname = NewStringf("arg%d", argnum);
      tempargname = 1;
    } else if (Strcmp(argname, "t") == 0 || Strcmp(argname, "T") == 0) {
      argname = NewStringf("t-arg%d", argnum);
      tempargname = 1;
    }
    if (Len(ffitype) > 0)
      Printf(args_placeholder, " (%s %s)", argname, ffitype);
    else
      Printf(args_placeholder, " %s", argname);

    if (Strcmp(ffitype, lispify_name(parent, lispy_name(Char(Getattr(parent, "sym:name"))), "'classname")) == 0)
      Printf(args_call, " (ff-pointer %s)", argname);
    else
      Printf(args_call, " %s", argname);

    Delete(ffitype);

    if (tempargname)
      Delete(argname);
  }

  // have to make a new string here, or else we'll mofiy "sym:name" permanantly during
  // the Replaceall -- definitely not what we want!
  String* sym_name2 = NewStringf("%s",Char(lispify_name(n, Getattr(n, "sym:name"), "'function")));

  Replaceall(sym_name2,NewStringf("__SWIG_"),NewStringf(""));

  String* defcfun_call = lispify_name(n, Getattr(n, "sym:name"), "'function");

  String* renamed_and_unscoped_method_name = NewStringf("new"); // Getattr(n,"sym:name");
  String* renamed_class_name               = Getattr(Getattr(n,"parentNode"),"classDeclaration:name");
  String* class_dot_method = NewStringf("%s.%s", renamed_class_name, renamed_and_unscoped_method_name);

  /* make it easy to figure out which methods are being called, and avoid conflicts between C++ over-ridden methods */
  Printf(f_clos, "(cl:defmethod %s (%s)\n  (%s%s))\n\n(cl:export '%s)\n\n",
         class_dot_method, //sym_name2,
	 args_placeholder,
	 defcfun_call,
	 args_call,
	 class_dot_method
	 );


  Printf(f_clos, "(cl:defmethod initialize-instance :after ((obj %s) &key%s)\n  (setf (slot-value obj 'ff-pointer) (%s%s)))\n\n",
         lispify_name(parent, lispy_name(Char(Getattr(parent, "sym:name"))), "'class"), args_placeholder,
         lispify_name(n, Getattr(n, "sym:name"), "'function"), args_call);

}

void CFFI::emit_setter(Node *n) {
  Node *parent = getCurrentClass();

  Printf(f_clos, "(cl:defmethod (cl:setf %s) (arg0 (obj %s))\n  (%s (ff-pointer obj) arg0))\n\n",
         //lispify_name(n, Getattr(n, "name"), "'method"),
	 NewStringf("method-%s",lispify_name(n, Getattr(n, "sym:name"), "'function")),
         lispify_name(parent, lispy_name(Char(Getattr(parent, "sym:name"))), "'class"), 
	 lispify_name(n, Getattr(n, "sym:name"), "'function"));
}


void CFFI::emit_getter(Node *n) {
  Node *parent = getCurrentClass();

  Printf(f_clos, "(cl:defmethod %s ((obj %s))\n  (%s (ff-pointer obj)))\n\n",
         //lispify_name(n, Getattr(n, "name"), "'method"),
	 NewStringf("method-%s",lispify_name(n, Getattr(n, "sym:name"), "'function")),
         lispify_name(parent, lispy_name(Char(Getattr(parent, "sym:name"))), "'class"), 
	 lispify_name(n, Getattr(n, "sym:name"), "'function"));
}

int CFFI::memberfunctionHandler(Node *n) {
  // Let SWIG generate a global forwarding function.
  Setattr(n, "cffi:memberfunction", "1");
  return Language::memberfunctionHandler(n);
}

int CFFI::membervariableHandler(Node *n) {
  // Let SWIG generate a get/set function pair.
  Setattr(n, "cffi:membervariable", "1");
  return Language::membervariableHandler(n);
}

int CFFI::functionWrapper(Node *n) {

  ParmList *parms = Getattr(n, "parms");
  String *iname = Getattr(n, "sym:name");
  Wrapper *f = NewWrapper();

  String *raw_return_type = Swig_typemap_lookup("ctype", n, "", 0);
  SwigType *return_type = Swig_cparse_type(raw_return_type);
  SwigType *resolved = SwigType_typedef_resolve_all(return_type);
  int is_void_return = (Cmp(resolved, "void") == 0);
  Delete(resolved);

  if (!is_void_return) {
    String *lresult_init = NewStringf("lresult = (%s)0", raw_return_type);
    Wrapper_add_localv(f, "lresult", raw_return_type, lresult_init, NIL);
    Delete(lresult_init);
  }

  String *overname = 0;
  if (Getattr(n, "sym:overloaded")) {
    overname = Getattr(n, "sym:overname");
  } else {
    if (!addSymbol(iname, n)) {
      DelWrapper(f);
      return SWIG_ERROR;
    }
  }

  String *wname = Swig_name_wrapper(iname);
  if (overname) {
    Append(wname, overname);
  }
  Setattr(n, "wrap:name", wname);

  // Emit all of the local variables for holding arguments.
  emit_parameter_variables(parms, f);

  // Attach the standard typemaps 
  Swig_typemap_attach_parms("ctype", parms, f);
  emit_attach_parmmaps(parms, f);

  int num_arguments = emit_num_arguments(parms);
  String *name_and_parms = NewStringf("%s (", wname);
  int i;
  Parm *p;
  int gencomma = 0;

#ifdef CFFI_DEBUG
  Printf(stderr, "function  -  %s - %d\n", Getattr(n, "name"), num_arguments);
#endif

  for (i = 0, p = parms; i < num_arguments; i++) {

    while (checkAttribute(p, "tmap:in:numinputs", "0")) {
      p = Getattr(p, "tmap:in:next");
    }

    SwigType *c_parm_type = Swig_cparse_type(Getattr(p, "tmap:ctype"));
    String *arg = NewStringf("l%s", Getattr(p, "lname"));

    // Emit parameter declaration
    if (gencomma)
      Printf(name_and_parms, ", ");
    String *parm_decl = SwigType_str(c_parm_type, arg);
    Printf(name_and_parms, "%s", parm_decl);
#ifdef CFFI_DEBUG
    Printf(stderr, "  param: %s\n", parm_decl);
#endif
    Delete(parm_decl);
    gencomma = 1;

    // Emit parameter conversion code
    String *parm_code = Getattr(p, "tmap:in");
    {
      Replaceall(parm_code, "$input", arg);
      Setattr(p, "emit:input", arg);
      Printf(f->code, "%s\n", parm_code);
      p = Getattr(p, "tmap:in:next");
    }

    Delete(arg);
  }
  Printf(name_and_parms, ")");

  // Emit the function definition
  String *signature = SwigType_str(return_type, name_and_parms);
  Printf(f->def, "EXPORT %s {", signature);
  Printf(f->code, "  try {\n");

  String *actioncode = emit_action(n);

  String *result_convert = Swig_typemap_lookup_out("out", n, "result", f, actioncode);
  Replaceall(result_convert, "$result", "lresult");
  Printf(f->code, "%s\n", result_convert);
  if(!is_void_return) Printf(f->code, "    return lresult;\n");
  Delete(result_convert);
  emit_return_variable(n, Getattr(n, "type"), f);

  Printf(f->code, "  } catch (...) {\n");
  if (!is_void_return)
    Printf(f->code, "    return (%s)0;\n", raw_return_type);
  Printf(f->code, "  }\n");
  Printf(f->code, "}\n");

  if (CPlusPlus)
    Wrapper_print(f, f_runtime);

  if (CPlusPlus) {
    emit_defun(n, wname);


    if (Getattr(n, "cffi:memberfunction"))
      emit_defmethod(n);
    else if (Getattr(n, "cffi:membervariable")) {
      if (Getattr(n, "memberget"))
        emit_getter(n);
      else if (Getattr(n, "memberset"))
        emit_setter(n);
    }
    else if (Getattr(n, "cffi:constructorfunction")) {
      emit_initialize_instance(n);
    }
  } else
    emit_defun(n, iname);

  //   if (!overloaded || !Getattr(n, "sym:nextSibling")) {
  //     update_package_if_needed(n);
  //     emit_buffered_defuns(n);
  //     // this is the last overload.
  //     if (overloaded) {
  //       emit_dispatch_defun(n);
  //     }
  //   }

  Delete(wname);
  DelWrapper(f);

  return SWIG_OK;
}


void CFFI::emit_defun(Node *n, String *name) {

  //   String *storage=Getattr(n,"storage");
  //   if(!storage || (Strcmp(storage,"extern") && Strcmp(storage,"externc")))
  //     return SWIG_OK;

  String *func_name = Getattr(n, "sym:name");

  ParmList *pl = Getattr(n, "parms");

  int argnum = 0;

  func_name = lispify_name(n, func_name, "'function");

  emit_inline(n, func_name);

  // append __SWIG_# if necessary to make the lisp symbols unique too,
  // where # is some number.
  char* match = strstr(Char(name),"__SWIG");
  if (match) {

    func_name = NewStringf("%s%s",func_name,match); // include all the way to the end so we capture the # (the important part for disambiguation) too.
    Setattr(n, "sym:name", func_name); // this fixes the -clos.lisp file of defmethod declarations.
  }


  Printf(f_cl, "\n(cffi:defcfun (\"%s\" %s)", name, func_name);

  String *ffitype = Swig_typemap_lookup("cout", n, ":pointer", 0);

  Printf(f_cl, " %s", ffitype);
  Delete(ffitype);

  for (Parm *p = pl; p; p = nextSibling(p), argnum++) {

    if (SwigType_isvarargs(Getattr(p, "type"))) {
      Printf(f_cl, "\n  %s", NewString("&rest"));
      continue;
    }

    String *argname = Getattr(p, "name");

    ffitype = Swig_typemap_lookup("cin", p, "", 0);

    int tempargname = 0;
    if (!argname) {

      argname = NewStringf("arg%d", argnum);
      tempargname = 1;
    } else if (Strcmp(argname, "t") == 0 || Strcmp(argname, "T") == 0) {
      argname = NewStringf("t_arg%d", argnum);
      tempargname = 1;
    }

    Printf(f_cl, "\n  (%s %s)", argname, ffitype);

    Delete(ffitype);

    if (tempargname)
      Delete(argname);
  }
  Printf(f_cl, ")\n");    /* finish arg list */

  emit_export(n, func_name);
}


int CFFI::constantWrapper(Node *n) {
  String *type = Getattr(n, "type");
  String *converted_value = convert_literal(Getattr(n, "value"), type);
  String *name = lispify_name(n, Getattr(n, "sym:name"), "'constant");

  if (Strcmp(name, "t") == 0 || Strcmp(name, "T") == 0)
    name = NewStringf("t_var");

  Printf(f_cl, "\n(cl:defconstant %s %s)\n", name, converted_value);
  Delete(converted_value);

  emit_export(n, name);
  return SWIG_OK;
}

int CFFI::variableWrapper(Node *n) {
  //  String *storage=Getattr(n,"storage");
  //  Printf(stdout,"\"%s\" %s)\n",storage,Getattr(n, "sym:name"));

  //  if(!storage || (Strcmp(storage,"extern") && Strcmp(storage,"externc")))
  //    return SWIG_OK;

  String *var_name = Getattr(n, "sym:name");
  String *lisp_type = Swig_typemap_lookup("cin", n, "", 0);
  String *lisp_name = lispify_name(n, var_name, "'variable");

  if (Strcmp(lisp_name, "t") == 0 || Strcmp(lisp_name, "T") == 0)
    lisp_name = NewStringf("t_var");

  Printf(f_cl, "\n(cffi:defcvar (\"%s\" %s)\n %s)\n", var_name, lisp_name, lisp_type);

  Delete(lisp_type);

  emit_export(n, lisp_name);
  return SWIG_OK;
}

int CFFI::typedefHandler(Node *n) {
  if (generate_typedef_flag && strncmp(Char(Getattr(n, "type")), "enum", 4)) {
    String *lisp_name = lispify_name(n, Getattr(n, "name"), "'typename");
    Printf(f_cl, "\n(cffi:defctype %s %s)\n", lisp_name, Swig_typemap_lookup("cin", n, "", 0));
    emit_export(n, lisp_name);
  }
  return Language::typedefHandler(n);
}

int CFFI::enumDeclaration(Node *n) {
  String *name = Getattr(n, "sym:name");
  bool slot_name_keywords;
  String *lisp_name = 0;
  if (name && Len(name) != 0) {
    lisp_name = lispify_name(n, name, "'enumname");
    if (GetFlag(n, "feature:bitfield")) {
      Printf(f_cl, "\n(cffi:defbitfield %s", lisp_name);
    } else {
      Printf(f_cl, "\n(cffi:defcenum %s", lisp_name);
    }
    slot_name_keywords = true;

    //Registering the enum name to the cin and cout typemaps
    Parm *pattern = NewParm(name, NULL, n);
    Swig_typemap_register("cin", pattern, lisp_name, NULL, NULL);
    Swig_typemap_register("cout", pattern, lisp_name, NULL, NULL);
    Delete(pattern);
    //Registering with the kind, i.e., enum
    pattern = NewParm(NewStringf("enum %s", name), NULL, n);
    Swig_typemap_register("cin", pattern, lisp_name, NULL, NULL);
    Swig_typemap_register("cout", pattern, lisp_name, NULL, NULL);
    Delete(pattern);

  } else {
    Printf(f_cl, "\n(defanonenum %s", name);
    slot_name_keywords = false;
  }

  for (Node *c = firstChild(n); c; c = nextSibling(c)) {

    String *slot_name = lispify_name(c, Getattr(c, "name"), "'enumvalue", slot_name_keywords);
    String *value = Getattr(c, "enumvalue");

    if (!value || GetFlag(n, "feature:bitfield:ignore_values"))
      Printf(f_cl, "\n\t%s", slot_name);
    else {
      String *type = Getattr(c, "type");
      String *converted_value = convert_literal(value, type);

      if (omit_readmacro_on_constants_flag) {
	//
	// We can't use the #. read-macro in some cases because it prevents 
	// self-references of previous values in an enum declaration.
	//
	// e.g. this won't work if we use #. :
	// enum BE { BEnone =     0, BEfallthru = 1, BEthrow =    2, BEany = (BEfallthru | BEthrow ), };
	//                                     self-referential labels ---->> ^^^^^^^^^^   ^^^^^^^
	// because it turns into the following in lisp, which the reader chokes on:
	//
	// (cffi:defcenum BE
	//    (:BEnone #.0)
	//    (:BEfallthru #.1)
	//    (:BEthrow #.2)
	//    (:BEany #.(cl:logior BEfallthru BEthrow)))
	//                         ^^^^^^^^^^ ^^^^^^^--reader doesn't know these symbols yet...???
	// Clozure Common Lisp reports:
	//       Error: Unbound variable: befallthru
	//
	//
	// Hence this next line,
	// Printf(f_cl, "\n\t(%s #.%s)", slot_name, converted_value);
	// has been changed to:

	Printf(f_cl, "\n\t(%s %s)", slot_name, converted_value); 

      } else {  // not -omit_readmacro_on_constants

	Printf(f_cl, "\n\t(%s #.%s)", slot_name, converted_value);

      }
      Delete(converted_value);
    }
    Delete(value);
  }

  Printf(f_cl, ")\n");

  // No need to export keywords
  if (lisp_name && Len(lisp_name) != 0) {
    emit_export(n, lisp_name);
  } else {
    for (Node *c = firstChild(n); c; c = nextSibling(c))
      emit_export(c, lispify_name(c, Getattr(c, "name"), "'enumvalue"));
  }

  return SWIG_OK;
}
void CFFI::emit_class(Node *n) {

#ifdef CFFI_WRAP_DEBUG
  Printf(stderr, "emit_class: ENTER... '%s'(%x)\n", Getattr(n, "sym:name"), n);
#endif

  String *name = Getattr(n, "sym:name");
  String *lisp_name = lispify_name(n, lispy_name(Char(name)), "'classname");

  String *bases = Getattr(n, "bases");
  String *supers = NewString("(");
  if (bases) {
    int first = 1;
    for (Iterator i = First(bases); i.item; i = Next(i)) {
      if (!first)
  Printf(supers, " ");
      // name by iteself can be wrong, doesn't accoutnt for rename: 
      String *sname = Getattr(i.item, "name");

      String *s = Getattr(i.item, "sym:name");
      if (s ==0) s = sname;

      Printf(supers, "%s", lispify_name(i.item, s, "'classname"));
    }
  } else {
    // Printf(supers,"ff:foreign-pointer");
  }

  Printf(supers, ")");
  Printf(f_clos, "\n(cl:defclass %s %s", lisp_name, supers);
  Printf(f_clos, "\n  ((ff-pointer :reader ff-pointer)))\n\n");

  Printf(f_clos, "(cl:export '%s)\n\n",lisp_name);


  Parm *pattern = NewParm(Getattr(n, "name"), NULL, n);

  Swig_typemap_register("lispclass", pattern, lisp_name, NULL, NULL);
  SwigType_add_pointer(Getattr(pattern, "type"));
  Swig_typemap_register("lispclass", pattern, lisp_name, NULL, NULL);
  SwigType_add_qualifier(Getattr(pattern, "type"), "const");
  Swig_typemap_register("lispclass", pattern, lisp_name, NULL, NULL);
  SwigType_del_pointer(Getattr(pattern, "type"));
  SwigType_add_reference(Getattr(pattern, "type"));
  Swig_typemap_register("lispclass", pattern, lisp_name, NULL, NULL);

#ifdef CFFI_WRAP_DEBUG
  Printf(stderr, "  pattern %s  name %s .. ... %s .\n", pattern, lisp_name);
#endif

  Delete(pattern);

  // Walk children to generate type definition.
  String *slotdefs = NewString("   ");

#ifdef CFFI_WRAP_DEBUG
  Printf(stderr, "  walking children...\n");
#endif

  Node *c;
  for (c = firstChild(n); c; c = nextSibling(c)) {
    String *storage_type = Getattr(c, "storage");
    if ((!Strcmp(nodeType(c), "cdecl") && (!storage_type || Strcmp(storage_type, "typedef")))) {
      String *access = Getattr(c, "access");

      // hack. why would decl have a value of "variableHandler" and now "0"?
      String *childDecl = Getattr(c, "decl");
      // Printf(stderr,"childDecl = '%s' (%s)\n", childDecl, Getattr(c,"view"));
      if (!Strcmp(childDecl, "0"))
  childDecl = NewString("");

      SwigType *childType = NewStringf("%s%s", childDecl,
               Getattr(c, "type"));
      String *cname = (access && Strcmp(access, "public")) ? NewString("nil") : Copy(Getattr(c, "name"));

      if (!SwigType_isfunction(childType)) {
  // Printf(slotdefs, ";;; member functions don't appear as slots.\n ");
  // Printf(slotdefs, ";; ");
  //        String *ns = listify_namespace(Getattr(n, "cffi:package"));
  String *ns = NewString("");
#ifdef CFFI_WRAP_DEBUG
  Printf(stderr, "slot name = '%s' ns = '%s' class-of '%s' and type = '%s'\n", cname, ns, name, childType);
#endif
  Printf(slotdefs, "(#.(swig-insert-id \"%s\" %s :type :slot :class \"%s\") %s)", cname, ns, name, childType);  //compose_foreign_type(childType)
  Delete(ns);
  if (access && Strcmp(access, "public"))
    Printf(slotdefs, " ;; %s member", access);

  Printf(slotdefs, "\n   ");
      }
      Delete(childType);
      Delete(cname);
    }
  }


  //   String *ns_list = listify_namespace(Getattr(n,"cffi:namespace"));
  //   update_package_if_needed(n,f_clhead);
  //   Printf(f_clos, 
  //          "(swig-def-foreign-class \"%s\"\n %s\n  (:%s\n%s))\n\n", 
  //          name, supers, kind, slotdefs);

  Delete(supers);
  //  Delete(ns_list);

  //  Parm *pattern = NewParm(name, NULL, n);
  // Swig_typemap_register("cin",pattern,lisp_name,NULL,NULL);  
  //Swig_typemap_register("cout",pattern,lisp_name,NULL,NULL);
  //Delete(pattern);

#ifdef CFFI_WRAP_DEBUG
  Printf(stderr, "emit_class: EXIT\n");
#endif
}

// Includes structs
void CFFI::emit_struct_union(Node *n, bool un = false) {
#ifdef CFFI_DEBUG
  Printf(stderr, "struct/union %s\n", Getattr(n, "name"));
  Printf(stderr, "struct/union %s\n and %s", Getattr(n, "kind"), Getattr(n, "sym:name"));
#endif

  String *name = Getattr(n, "sym:name");
  String *kind = Getattr(n, "kind");

  if (Strcmp(kind, "struct") != 0 && Strcmp(kind, "union") != 0) {
    Printf(stderr, "Don't know how to deal with %s kind of class yet.\n", kind);
    Printf(stderr, " (name: %s)\n", name);
    SWIG_exit(EXIT_FAILURE);
  }
  String *lisp_name = lispify_name(n, name, "'classname");

  //Register the struct/union name to the cin and cout typemaps

  Parm *pattern = NewParm(name, NULL, n);
  Swig_typemap_register("cin", pattern, lisp_name, NULL, NULL);
  Swig_typemap_register("cout", pattern, lisp_name, NULL, NULL);
  Delete(pattern);
  //Registering with the kind, i.e., struct or union
  pattern = NewParm(NewStringf("%s %s", kind, name), NULL, n);
  Swig_typemap_register("cin", pattern, lisp_name, NULL, NULL);
  Swig_typemap_register("cout", pattern, lisp_name, NULL, NULL);
  Delete(pattern);

  if (un) {
    Printf(f_cl, "\n(cffi:defcunion %s", lisp_name);
  } else
    Printf(f_cl, "\n(cffi:defcstruct %s", lisp_name);


  for (Node *c = firstChild(n); c; c = nextSibling(c)) {
#ifdef CFFI_DEBUG
    Printf(stderr, "struct/union %s\n", Getattr(c, "name"));
    Printf(stderr, "struct/union %s and %s \n", Getattr(c, "kind"), Getattr(c, "sym:name"));
#endif

    if (Strcmp(nodeType(c), "cdecl")) {
      //C declaration ignore
      //        Printf(stderr, "Structure %s has a slot that we can't deal with.\n",
      //               name);
      //        Printf(stderr, "nodeType: %s, name: %s, type: %s\n", 
      //               nodeType(c),
      //               Getattr(c, "name"),
      //               Getattr(c, "type"));
      //       SWIG_exit(EXIT_FAILURE);
    } else {
      SwigType *childType = NewStringf("%s%s", Getattr(c, "decl"), Getattr(c, "type"));

      Node *node = NewHash();
      Setattr(node, "type", childType);
      Setfile(node, Getfile(n));
      Setline(node, Getline(n));
      const String *tm = Swig_typemap_lookup("cin", node, "", 0);

      String *typespec = tm ? NewString(tm) : NewString("");

      String *slot_name = lispify_name(c, Getattr(c, "sym:name"), "'slotname");
      if (Strcmp(slot_name, "t") == 0 || Strcmp(slot_name, "T") == 0)
	slot_name = NewStringf("t_var");

      Printf(f_cl, "\n\t(%s %s)", slot_name, typespec);

      Delete(node);
      Delete(childType);
      Delete(typespec);
    }
  }

  Printf(f_cl, ")\n");

  emit_export(n, lisp_name);
  for (Node *child = firstChild(n); child; child = nextSibling(child)) {
    if (!Strcmp(nodeType(child), "cdecl")) {
      emit_export(child, lispify_name(child, Getattr(child, "sym:name"), "'slotname"));
    }
  }

  /* Add this structure to the known lisp types */
  //Printf(stdout, "Adding %s foreign type\n", name);
  //  add_defined_foreign_type(name);

}

void CFFI::emit_export(Node *n, String *name) {
  if (GetInt(n, "feature:export"))
    Printf(f_cl, "\n(cl:export '%s)\n", name);
}

void CFFI::emit_inline(Node *n, String *name) {
  if (GetInt(n, "feature:inline"))
    Printf(f_cl, "\n(cl:declaim (cl:inline %s))\n", name);
}

String *CFFI::lispify_name(Node *n, String *ty, const char *flag, bool kw) {
  String *intern_func = Getattr(n, "feature:intern_function");
  if (intern_func) {
    if (Strcmp(intern_func, "1") == 0)
      intern_func = NewStringf("swig-lispify");
    return NewStringf("#.(%s \"%s\" %s%s)", intern_func, ty, flag, kw ? " :keyword" : "");
  } else if (kw)
    return NewStringf(":%s", ty);
  else
    return ty;
}

/* utilities */
/* returns new string w/ parens stripped */
String *CFFI::strip_parens(String *string) {
  char *s = Char(string), *p;
  int len = Len(string);
  String *res;

  if (len == 0 || s[0] != '(' || s[len - 1] != ')') {
    return NewString(string);
  }

  p = (char *) malloc(len - 2 + 1);
  if (!p) {
    Printf(stderr, "Malloc failed\n");
    SWIG_exit(EXIT_FAILURE);
  }

  strncpy(p, s + 1, len - 1);
  p[len - 2] = 0;   /* null terminate */

  res = NewString(p);
  free(p);

  return res;
}

String *CFFI::trim(String *str) {
  char *c = Char(str);
  while (*c != '\0' && isspace((int) *c))
    ++c;
  String *result = NewString(c);
  Chop(result);
  return result;
}

String *CFFI::infix_to_prefix(String *val, char split_op, const String *op, String *type) {
  List *ored = Split(val, split_op, -1);

  // some float hackery
  //i don't understand it, if you do then please explain
  //   if ( ((split_op == '+') || (split_op == '-')) && Len(ored) == 2 &&
  //        (SwigType_type(type) == T_FLOAT || SwigType_type(type) == T_DOUBLE ||
  //    SwigType_type(type) == T_LONGDOUBLE) ) {
  //     // check that we're not splitting a float
  //     String *possible_result = convert_literal(val, type, false);
  //     if (possible_result) return possible_result;

  //   }

  // try parsing the split results. if any part fails, kick out.
  bool part_failed = false;
  if (Len(ored) > 1) {
    String *result = NewStringf("(%s", op);
    for (Iterator i = First(ored); i.item; i = Next(i)) {
      String *converted = convert_literal(i.item, type);
      if (converted) {
  Printf(result, " %s", converted);
  Delete(converted);
      } else {
  part_failed = true;
  break;
      }
    }
    Printf(result, ")");
    Delete(ored);
    return part_failed ? 0 : result;
  } else {
    Delete(ored);
  }
  return 0;
}

/* To be called by code generating the lisp interface
   Will return a String containing the literal based on type.
   Will return null if there are problems.

   try_to_split defaults to true (see stub above).
*/
String *CFFI::convert_literal(String *literal, String *type, bool try_to_split) {
  String *num_param = Copy(literal);
  String *trimmed = trim(num_param);
  String *num = strip_parens(trimmed), *res = 0;
  Delete(trimmed);
  char *s = Char(num);

  // very basic parsing of infix expressions.
  if (try_to_split) {
    if ((res = infix_to_prefix(num, '|', "cl:logior", type)))
      return res;
    if ((res = infix_to_prefix(num, '&', "cl:logand", type)))
      return res;
    if ((res = infix_to_prefix(num, '^', "cl:logxor", type)))
      return res;
    if ((res = infix_to_prefix(num, '*', "cl:*", type)))
      return res;
    if ((res = infix_to_prefix(num, '/', "cl:/", type)))
      return res;
    if ((res = infix_to_prefix(num, '+', "cl:+", type)))
      return res;
    if ((res = infix_to_prefix(num, '-', "cl:-", type)))
      return res;
  }

  if (SwigType_type(type) == T_FLOAT || SwigType_type(type) == T_DOUBLE || SwigType_type(type) == T_LONGDOUBLE) {
    // Use CL syntax for float literals 

    // careful. may be a float identifier or float constant.
    char *num_start = Char(num);
    char *num_end = num_start + strlen(num_start) - 1;

    bool is_literal = isdigit(*num_start) || (*num_start == '.') || (*num_start == '+') || (*num_start == '-');

    String *lisp_exp = 0;
    if (is_literal) {
      if (*num_end == 'f' || *num_end == 'F') {
        lisp_exp = NewString("f");
      } else {
        lisp_exp = NewString("d");
      }

      if (*num_end == 'l' || *num_end == 'L' || *num_end == 'f' || *num_end == 'F') {
        *num_end = '\0';
        num_end--;
      }

      int exponents = Replaceall(num, "e", lisp_exp) + Replaceall(num, "E", lisp_exp);

      if (!exponents)
        Printf(num, "%s0", lisp_exp);

      if (exponents > 1 || (exponents + Replaceall(num, ".", ".") == 0)) {
        Delete(num);
        num = 0;
      }
    }
    return num;
  } else if (SwigType_type(type) == T_CHAR) {
    /* Use CL syntax for character literals */
    String* result = NewStringf("#\\%c", s[2]);
    Delete(num);
    //    Printf(stderr, "%s  %c %d", s, s[2], s);
    return result;
  } else if (SwigType_type(type) == T_STRING) {
    /* Use CL syntax for string literals */
    String* result = NewStringf("\"%s\"", num_param);
    Delete(num);
    return result;
  } else if (SwigType_type(type) == T_INT || SwigType_type(type) == T_UINT) {
    // Printf(stderr, "Is a T_INT or T_UINT %s, before replaceall\n", s);

    // the following code is problematic because enum constants are getting
    // converted along with numbers...
    /*
enum BE
{
    BEnone =     0,
    BEfallthru = 1,
    BEthrow =    2,
    BEreturn =   4,
    BEgoto =     8,
    BEhalt =     0x10,
    BEbreak =    0x20,
    BEcontinue = 0x40,
    BEany = (BEfallthru | BEthrow | BEreturn | BEgoto | BEhalt),
};


(cffi:defcenum BE
	(:BEnone #.0)
	(:BEfallthru #.1)
	(:BEthrow #.2)
	(:BEreturn #.4)
	(:BEgoto #.8)
	(:BEhalt #.#x10)
	(:BEbreak #.#x20)
	(:BEcontinue #.#x40)
	(:BEany #.(cl:logior BEfathr BEthrow BEretrn BEgoto BEhat))) <---- note the missing u's and l's in these constant references, which stops the lisp compiler in its tracks.
     */

    // so if our "number" num starts with a letter, we should skip the u and l replaceall calls.
  if (s && isalpha(s[0])) {
	/* don't do the Replaceall calls */
	//Printf(stderr, "swig CFFI::convert_literal() report: Skipping-U-and-L-elimination for constant num: '%s'.\n", s);	
      } else {
        //Printf(stderr, "swig CFFI::convert_literal() report: Doing-the-U-and-L-elimination for constant num: '%s'.\n", s);	

	Replaceall(num, "u", "");
	Replaceall(num, "U", "");
	Replaceall(num, "l", "");
	Replaceall(num, "L", "");
      }

    int i, j;
    if (sscanf(s, "%d >> %d", &i, &j) == 2) {
      String* result = NewStringf("(cl:ash %d -%d)", i, j);
      Delete(num);
      return result;
    } else if (sscanf(s, "%d << %d", &i, &j) == 2) {
      String* result = NewStringf("(cl:ash %d %d)", i, j);
      Delete(num);
      return result;
    }
  }

  if (Len(num) >= 2 && s[0] == '0') { /* octal or hex */
    if (s[1] == 'x'){
      DohReplace(num,"0","#",DOH_REPLACE_FIRST);
    }
    else{
      // turn this off...it's not doing the right thing
      // DohReplace(num,"0","#o",DOH_REPLACE_FIRST);
    }
  }
  // but do strip off trailing indicators from C/C++
  Replaceall(num, "u", "");
  Replaceall(num, "U", "");
  Replaceall(num, "l", "");
  Replaceall(num, "L", "");
  
  return num;
}


// don't convert names in C: we may *want case sensitivity* in our Lisp code!!!
// so lispy_name() is now a no-op.
String *CFFI::lispy_name(char *name) {
    return NewString(name);
}

#if 0
// Strongly depracated:
// the orignal lispy_name(), here so that nobody thinks of reintroducing it: DON'T USE IT!! 
// Doing cutesy name manging like this messes up
// the differentiation between case sensitive objects. 
//
//less flexible as it does the conversion in C, the lispify name does the conversion in lisp
String *CFFI::old_bad_lispy_name(char *name) {
  bool helper = false;
  String *new_name = NewString("");
  for (unsigned int i = 0; i < strlen(name); i++) {
    if (name[i] == '_' || name[i] == '-') {
      Printf(new_name, "%c", '-');
      helper = false;
    } else if (name[i] >= 'A' && name[i] <= 'Z') {
      if (helper)
	Printf(new_name, "%c", '-');
      Printf(new_name, "%c", ('a' + (name[i] - 'A')));
      helper = false;
    } else {
      helper = true;
      Printf(new_name, "%c", name[i]);
    }
  }
  return new_name;
}
#endif

extern "C" Language *swig_cffi(void) {
  return new CFFI();
}
