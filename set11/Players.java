// Constructor template for Players:
//     Players.make(String name)
//
// Interpretation:  represents a member of a team. Player objects are mutable
// because their status can change without changing the identity of the Player.

public class Players {

    // Returns a player with the given name, under contract, with an uninjured
    // and non-suspended status

    public static Player make (String name) {
        return new Player1 (name);
    }

}
