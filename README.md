# The Ur/Web People Organizer

Here lies a library of [Ur/Web](http://www.impredicative.com/ur/) code for rapid construction of web applications for organizing people and events in different ways.  For a quick sense of what's going on here, see [the app showcase site](http://upo.csail.mit.edu/).

No doubt more documentation will materialize here soon.  For now, see the `Ui` module (e.g., in file `ui.urs`) for definitions of the basic concepts underlying UPO.  A quick tour of the big ideas:
* A value of type `Ui.t a` is a **first-class UI element**, ready to drop onto a page without any further fuss.  The type parameter `a` stands for the private state of such a component.
* A module of type `Ui.S` or `Ui.S0` packages a UI element as a module rather than a value, for cases where that's a more convenient interface.
* A number of basic **combinators** are available to compose UI elements.  For instance, `Ui.seq` lays out a number of elements in sequence, and `Ui.const` produces a (boring) UI element that just displays some fixed HTML.
* The easiest way to bring a (usually composite) UI element to life is to convert it into a page with `Ui.simple`, which just takes one UI element as a parameter.
* Fancier pages include **several tabs** for selecting different full-page UI elements.  The function `Ui.tabbed` creates such a page out of a tuple of elements, each with an optional title.  (Tabs lacking titles are hidden.)
* Finally, pretty Bootstrap **modal dialogs** are supported via function `Ui.modalButton` that creates HTML for the button, given a computation that decides which HTML to display in the dialog when the button is clicked.  Function `Ui.modal` is useful for creating that content.

Those are the basics!  The other modules of the library define more complex UI elements, usually parameterized in a variety of ways.  See the `examples` directory, which includes the [showcase apps](http://upo.csail.mit.edu/) and some simpler demos.

## Installation

Two other Ur/Web libraries are required: [Bootstrap](https://github.com/urweb/bootstrap) and [AjaxUpload](https://github.com/urweb/ajaxUpload).
Here is one sequence of commands to set up these and some dependencies used in certain examples (but not required for UPO itself).

        # If you don't already have Ur/Web itself installed, run these steps.
        # In general, a working version may be required, as UPO may use features
        # beyond what the latest release supports.
        git clone https://github.com/urweb/urweb.git
        cd urweb
        ./autogen.sh
        ./configure
        make
        sudo make install
        cd ..

        # Library required for UPO
        git clone https://github.com/urweb/ajaxUpload.git
        cd ajaxUpload
        ./autogen.sh
        ./configure
        make
        sudo make install
        cd ..

        # _Optional_ library, used in some examples
        git clone https://github.com/urweb/world.git
        cd world
        ./autogen.sh
        ./configure
        make
        sudo make install
        cd ..

        # _Optional_ library, used in some examples
        git clone https://github.com/urweb/email.git
        cd email
        ./autogen.sh
        ./configure
        make
        sudo make install
        cd ..

        # Library required for UPO
        git clone https://github.com/urweb/bootstrap.git
        sudo ln -s `pwd`/bootstrap /usr/local/lib/urweb/ur/

        # This library itself, for good measure
        git clone https://github.com/urweb/upo.git
        sudo ln -s `pwd`/upo /usr/local/lib/urweb/ur/

        # Building one example
        cd upo/examples
        cp onlineConfSecret_template.ur onlineConfSecret.ur
        # Read above file and fill in parameters.
        urweb onlineconf
        createdb onlineconf
        psql -f onlineconf.sql onlineconf
        ./onlineconf.exe
