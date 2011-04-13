/* File : example.i */
%module example

// gdc is predefined in Clozure CL, so to avoid
// name conflict, we must use %rename(new_name) old_name;
%rename(custom_gcd) gcd;

%feature("export");

%inline %{
extern int    gcd(int x, int y);
extern double Foo;
%}
