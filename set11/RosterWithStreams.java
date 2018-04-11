// Constructor template for RosterWithStreams:
//      RosterWithStreams.empty()

// Interpretation:  represents a set of players. RosterWithStreams objects are
// immutable, but all players on a roster have mutable status, which can affect
// the values returned by the readyCount() and readyRoster() methods.

import java.util.*;

public class RosterWithStreams {

    // Returns an empty roster

    public static RosterWithStream empty () {
        return new RosterWithStream1(new ArrayList<>());
    }

}
