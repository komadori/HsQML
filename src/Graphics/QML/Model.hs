{-| Facility for working with Qt data models.

This module is a placeholder for the ability to freely define QML classes which
implement the @QAbstractItemModel@ interface and functions to interact with
those models. This is not currently supported, but the functionality will be
available in a future release.

HsQML does currently provide one mechanism for creating a Qt data model, the
@AutoListModel@ item. This item implements the @QAbstractItemModel@ interface
and provides a QML-side solution for generating a stateful Qt data model from a
succession of JavaScript arrays.

The advantage of this over using the arrays directly is that, when the array
changes, the @AutoListModel@ can generate item add, remove, and change events
based on the differences between the old and new arrays. An simple array
binding, on the other hand, causes the entire model to be reset so that views
lose their state, cannot animate changes, etc.

To use this facility, you must assign an @AutoListModel@ item to the @model@
property of a QML view such as a @Repeater@ item. In turn, the @source@
property of the @AutoListModel@ must then be bound to an expression yielding
the arrays you want to use. The @AutoListModel@ can be imported from the
@HsQML.Model 1.0@ module using an @import@ statement in your QML script and it
has several properties which can be set from QML as described below:

[@mode@] Specifies how the model is updated when the source array changes.
Possible values are:

    [@AutoListModel.ByReset@] The model is reset entirely (default). 
    [@AutoListModel.ByIndex@] The elements in the old and new arrays are
    compared for equality and change signals are sent for any that aren't
    equal. If the length of the array has changed then elements will be added
    to or removed from the end of the list model. 
    [@AutoListModel.ByKey@] The key function is applied to each element in the
    old and new arrays. According to the key, elements which exist in both the
    old and new arrays are moved to their new positions in the list model as
    necessary, elements which only exist in the new array are added, and ones
    which only existed in the old are removed. Where elements have been moved,
    the old and new values are compared for equality and change signals are
    sent for any that aren't equal. Unlike the other modes which only require
    linear time to process the arrays, if elements are reordered then this mode
    requires quadratic time in the worst case.
    [@AutoListModel.ByKeyNoReorder@] This is the same as the ByKey mode except
    that the model will not explicitly move elements to different positions,
    only remove and re-add them at their new location as necessary.

[@source@] JavaScript array containing the elements for the model.
[@equalityTest@] JavaScript function which takes two parameters, compares them
for equality, and returns a boolean. This is used by all the modes except
@ByReset@. If no function is set then the model will use the JavaScript equals
operator by default.
[@keyFunction@] JavaScript function which takes a single parameter and
returns a string key for use by the @ByKey@ and @ByKeyNoReorder@ modes. If no
function is set then the model will use JavaScript's string conversion by
default.
-}
module Graphics.QML.Model () where
