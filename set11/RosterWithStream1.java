
// Constructor template for RosterWithStream1:
//      new RosterWithStream1(List<Player> givenPlayers)

// Interpretation:  represents a set of players. RosterWithStream1 objects are
// immutable, but all players on a roster have mutable status, which can affect
// the values returned by the readyCount() and readyRoster() methods.

import java.util.stream.Stream;
import java.util.stream.Collectors;
import java.util.*;

public class RosterWithStream1 implements RosterWithStream {

    private List<Player> players; // the players on this roster

    RosterWithStream1 (List<Player> givenPlayers) {
        this.players = givenPlayers;
    }

    // Returns a roster consisting of the given player together
    // with all players on this roster.
    // Example:
    //     r.with(p).with(p)  =>  r.with(p)

    public RosterWithStream  with (Player p) {
        List<Player> updatedPlayers = new ArrayList<>(this.players);
        if (!this.has(p))
            updatedPlayers.add(p);
        return new RosterWithStream1(updatedPlayers);
    }

    // Returns a roster consisting of all players on this roster
    // except for the given player.
    // Examples:
    //     RosterWithStreams.empty().without(p)  =>  RosterWithStreams.empty()
    //     r.without(p).without(p)     =>  r.without(p)

    public RosterWithStream without (Player p) {
        List<Player> updatedPlayers = new ArrayList<>(this.players);
        updatedPlayers.remove(p);
        return new RosterWithStream1(updatedPlayers);
    }

    // Returns true iff the given player is on this roster.
    // Examples:
    //
    //     RosterWithStreams.empty().has(p)  =>  false
    //
    // If r is any roster, then
    //
    //     r.with(p).has(p)     =>  true
    //     r.without(p).has(p)  =>  false

    public boolean has (Player p) {
        return this.players.contains(p);
    }

    // Returns the number of players on this roster.
    // Examples:
    //
    //     RosterWithStreams.empty().size()  =>  0
    //
    // If r is a roster with r.size() == n, and r.has(p) is false, then
    //
    //     r.without(p).size()          =>  n
    //     r.with(p).size()             =>  n+1
    //     r.with(p).with(p).size()     =>  n+1
    //     r.with(p).without(p).size()  =>  n

    public int size () {
        return this.players.size();
    }

    // Returns the number of players on this roster whose current
    // status indicates they are available.

    public int readyCount () {
        return (int) this.players.stream()
                .filter(Player::available)
                .count();
    }

    // Returns a roster consisting of all players on this roster
    // whose current status indicates they are available.

    public RosterWithStream readyRoster () {
        List<Player> readyPlayers = this.players.stream()
                .filter(Player::available)
                .collect(Collectors.toList());
        return new RosterWithStream1(readyPlayers);
    }

    // Returns an iterator that generates each player on this
    // roster exactly once, in alphabetical order by name.

    public Iterator<Player> iterator () {
        List<Player> iteratorPlayers = new ArrayList<>(this.players);
        iteratorPlayers.sort(Comparator.comparing(Player::name));
        return iteratorPlayers.iterator();
    }

    // Returns a sequential Stream with this RosterWithStream
    // as its source.
    // The result of this method generates each player on this
    // roster exactly once, in alphabetical order by name.
    // Examples:
    //
    //     RosterWithStreams.empty().stream().count()  =>  0
    //
    //     RosterWithStreams.empty().stream().findFirst().isPresent()
    //         =>  false
    //
    //     RosterWithStreams.empty().with(p).stream().findFirst().get()
    //         =>  p
    //
    //     this.stream().distinct()  =>  true
    //
    //     this.stream().filter((Player p) -> p.available()).count()
    //         =>  this.readyCount()

    public Stream<Player> stream () {
        return this.players.stream();
    }


    // RETURNS:  a string representation of this Rosters object
    // WHERE:  if r1 and r2 are rosters of different sizes, then r1.toString()
    // is not the same string as r2.toString().

    public String toString () {
        String RosterString = "Rosters" + "\n";
        if (this.players.size() == 0) {
            RosterString = "RostersEmpty";
        }
        for (Player p : this.players) {
            RosterString = RosterString + "[" + p.toString() + "]" + "\n";
        }
        return RosterString;
    }

    // GIVEN:  an object
    // RETURNS:  true iff this Rosters is equal to the given object.
    // WHERE:  if r1 and r2 are rosters, then r1.equals(r2) if and only if
    // every player on roster r1 is also on roster r2, and every player on
    // roster r2 is also on roster r1.

    public boolean equals (Object o) {

        if (o instanceof RosterWithStream1) {

            RosterWithStream1 that = (RosterWithStream1) o;

            for (Player p : that.players) {
                if (!(this.players.contains(p))) {
                    return false;
                }
            }

            if (!(this.size() == that.size()))
                return false;

            return true;
        } else
            return false;
    }

    // RETURNS:  the hashCode of this Rosters object
    // WHERE:  if r is a roster, then r.hashCode() always returns the same
    // value, even if r has some players whose status changes.

    public int hashCode() {
        int hash = 0;
        for (Player p : this.players) {
            hash = hash + p.hashCode();

        }
        return hash;
    }
}
