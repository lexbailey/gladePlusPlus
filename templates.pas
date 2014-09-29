unit templates;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  outputTLHeader =
   '#ifndef GLADE_PLUS_PLUS_%s_INCLUDE'+#10
  +'#define GLADE_PLUS_PLUS_%s_INCLUDE'+#10
  +'#include <gtkmm.h>'+#10
  +'#include "%s_signals.gpp.hpp"'+#10
  +#10
  +#9+'class GladePlusPlus%s{'+#10
  +#9+#9+'public:'+#10;

  outputTLFooter =
    #9+#9+#9+'GladePlusPlus%s(Glib::RefPtr<Gtk::Builder> refBuilder);'+#10
   +#9+#9+#9+'~GladePlusPlus%s();'+#10
+#9+'};'+#10

+'#endif'+#10;




  outputTLSourceHeader =
   '#include "%s"'+#10
  +#10
  +'GladePlusPlus%s::GladePlusPlus%s(Glib::RefPtr<Gtk::Builder> refBuilder){'+#10;

  outputTLSourceMiddle =
   #10
  +'}'+#10
  +#10
  +'GladePlusPlus%s::~GladePlusPlus%s(){'+#10;

  outputTLSourceFooter =
    #10+'}'+#10;








  topLevelHeader =
   '#ifndef GLADE_PLUS_PLUS_TOP_LEVEL_%s_INCLUDE'+#10
  +'#define GLADE_PLUS_PLUS_TOP_LEVEL_%s_INCLUDE'+#10
  +'#include <gtkmm.h>'+#10
  +'#include "%s_signals.gpp.hpp"';

  topLevelPostInclude =
   #10
  +#9+'class GladePlusPlus%s{'+#10
  +#9+#9+'public:'+#10
  +#9+#9+#9+'Glib::RefPtr<Gtk::Application> app;'+#10;


  topLevelFooter =
    #9+#9+#9+'GladePlusPlus%s(int argc, char **argv);'+#10
   +#9+#9+#9+'~GladePlusPlus%s();'+#10
+#9+'};'+#10

+'#endif'+#10;

  topLevelSourceHeader =
   '#include "%s"'+#10
  +#10
  +'GladePlusPlus%s::GladePlusPlus%s(int argc, char **argv){'+#10
  +#9+'this->app = Gtk::Application::create(argc, argv, "%s");'+#10
  +#9+'bool hasError = false;'+#10
  +#9+'//Load the GtkBuilder file and instantiate its widgets:'+#10
  +#9+'Glib::RefPtr<Gtk::Builder> refBuilder = Gtk::Builder::create();'+#10
  +#9+'try{'+#10
  +#9+#9+'refBuilder->add_from_file("%s");'+#10
  +#9+'}'+#10
  +#9+'catch(const Glib::FileError& ex){'+#10
  +#9+#9+'fprintf(stderr, "File error: %%s\n", ex.what().data());'+#10
  +#9+#9+'hasError = true;'+#10
  +#9+'}'+#10
  +#9+'catch(const Glib::MarkupError& ex){'+#10
  +#9+#9+'fprintf(stderr, "Markup error: %%s\n", ex.what().data());'+#10
  +#9+#9+'hasError = true;'+#10
  +#9+'}'+#10
  +#9+'catch(const Gtk::BuilderError& ex){'+#10
  +#9+#9+'fprintf(stderr, "Builder error: %%s\n", ex.what().data());'+#10
  +#9+#9+'hasError = true;'+#10
  +#9+'}'+#10
  +#10
  +#9+'if (!hasError){'+#10;

  topLevelSourceMiddle =
   #10
  +#9+'}'+#10
  +'}'+#10
  +#10
  +'GladePlusPlus%s::~GladePlusPlus%s(){'+#10;

  topLevelSourceFooter =
    #10+'}'+#10;

  signalsHeader =
     '#ifndef GLADE_PLUS_PLUS_%s_SIGNALS_INCLUDE'+#10
    +'#define GLADE_PLUS_PLUS_%s_SIGNALS_INCLUDE'+#10
    +'#include <gtkmm.h>';
  signalsFooter =
    '#endif' + #10;

implementation

end.

