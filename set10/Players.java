// Constructor template for Players:
//     Players.make(String name)
//
// Interpretation:  represents a member of a team. Player objects are mutable
// because their status can change without changing the identity of the Player.

import java.util.*;

public class Players implements Player {

    String name;           // the name of the player
    Boolean underContract; // true if the player is under contract
    Boolean isInjured;     // true if the player is injured
    Boolean isSuspended;   // true if the player is suspended

    private Players (String s) {
        this.name = s;
        this.underContract = true;
        this.isInjured = false;
        this.isSuspended = false;
    }

    // Returns a player with the given name, under contract, with an uninjured
    // and non-suspended status

    public static Player make (String name) {
        return new Players (name);
    }

    // Returns the name of this player.
    // Example:
    //     Players.make("Gordon Wayhard").name()  =>  "Gordon Wayhard"

    public String name () {
        return this.name;
    }

    // Returns true iff this player is
    //     under contract, and
    //     not injured, and
    //     not suspended
    // Example:
    //     Player gw = Players.make ("Gordon Wayhard");
    //     System.out.println (gw.available());  // prints true
    //     gw.changeInjuryStatus (true);
    //     System.out.println (gw.available());  // prints false

    public boolean available () {
        return underContract && !isInjured && !isSuspended;
    }

    // Returns true iff this player is under contract (employed).
    // Example:
    //     Player ih = Players.make ("Isaac Homas");
    //     System.out.println (ih.underContract());  // prints true
    //     ih.changeContractStatus (false);
    //     System.out.println (ih.underContract());  // prints false
    //     ih.changeContractStatus (true);
    //     System.out.println (ih.underContract());  // prints true

    public boolean underContract () {
        return underContract;
    }

    // Returns true iff this player is injured.

    public boolean isInjured () {
        return isInjured;
    }

    // Returns true iff this player is suspended.

    public boolean isSuspended () {
        return isSuspended;
    }

    // Changes the underContract() status of this player
    // to the specified boolean.

    public void changeContractStatus (boolean newStatus) {
        this.underContract = newStatus;
    }

    // Changes the isInjured() status of this player
    // to the specified boolean.

    public void changeInjuryStatus (boolean newStatus) {
        this.isInjured = newStatus;
    }

    // Changes the isSuspended() status of this player
    // to the specified boolean.

    public void changeSuspendedStatus (boolean newStatus) {
        this.isSuspended = newStatus;
    }

    // RETURNS:  a string representation of this Players object
    // WHERE:  if p1 and p2 are players with distinct names, then
    // p1.toString() is not the same string as p2.toString().

    public String toString () {
        return this.name +
                "[" + this.underContract.toString() + "]" +
                "[" + this.isInjured.toString() + "]" +
                "[" + this.isSuspended.toString() + "]";
    }

    // RETURNS:  the hashCode of this Players object
    // WHERE:  if p is a player, then p.hashCode() always returns the same
    // value, even after the player's status is changed by calling one of the
    // last three methods listed below.

    public int hashCode() {
        return Objects.hash(this.name);
    }

}
