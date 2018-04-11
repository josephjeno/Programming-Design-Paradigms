// Constructor template for Rosters:
//      Rosters.empty()

// Interpretation:  represents a set of players. Roster objects are immutable,
// but all players on a roster have mutable status, which can affect the values
// returned by the readyCount() and readyRoster() methods.

import java.util.*;

public class Rosters implements Roster {

    List<Player> players; // the players on this roster

    private Rosters (List<Player> givenPlayers) {
        this.players = givenPlayers;
    }

    // Returns an empty roster

    public static Roster empty () {
        return new Rosters(new ArrayList<>());
    }

    // Returns a roster consisting of the given player together
    // with all players on this roster.
    // Example:
    //     r.with(p).with(p)  =>  r.with(p)

    public Roster with (Player p) {
        List<Player> updatedPlayers = new ArrayList<>();
        updatedPlayers.addAll(this.players);
        if (!this.has(p)) {
            updatedPlayers.add(p);
        }
        return new Rosters(updatedPlayers);
    }

    // Returns a roster consisting of all players on this roster
    // except for the given player.
    // Examples:
    //     Rosters.empty().without(p)  =>  Rosters.empty()
    //     r.without(p).without(p)     =>  r.without(p)

    public Roster without (Player p) {
        List<Player> updatedPlayers = new ArrayList<>();
        updatedPlayers.addAll(this.players);
        updatedPlayers.remove(p);
        return new Rosters(updatedPlayers);
    }

    // Returns true iff the given player is on this roster.
    // Examples:
    //
    //     Rosters.empty().has(p)  =>  false
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
    //     Rosters.empty().size()  =>  0
    //
    // If r is a roster with r.size() == n, and r.has(p) is false, then
    //
    //     r.without(p).size()          =>  n
    //     r.with(p).size               =>  n+1
    //     r.with(p).with(p).size       =>  n+1
    //     r.with(p).without(p).size()  =>  n

    public int size () {
        return this.players.size();
    }

    // Returns the number of players on this roster whose current
    // status indicates they are available.

    public int readyCount () {
        int available = 0;
        for (Player p : this.players) {
            if (p.available()) {
                available = available + 1;
            }
        }
        return available;
    }

    // Returns a roster consisting of all players on this roster
    // whose current status indicates they are available.

    public Roster readyRoster () {
        List<Player> readyPlayers = new ArrayList<>();
        for (Player p : this.players) {
            if (p.available()) {
                readyPlayers.add(p);
            }
        }
        return new Rosters(readyPlayers);
    }

    // Returns an iterator that generates each player on this
    // roster exactly once, in alphabetical order by name.

    public Iterator<Player> iterator () {
        List<Player> uniquePlayers = new ArrayList<>();
        List<Player> finalRoster = new ArrayList<>();
        List<String> uniqueNames = new ArrayList<>();

        for (Player p : this.players) {
            if (!uniquePlayers.contains(p)) {
                uniquePlayers.add(p);
                uniqueNames.add(p.name());
            }
        }

        Collections.sort(uniqueNames);

        for (String n : uniqueNames) {
            Player current = Players.make("void");

            for (Player p : uniquePlayers) {
                if (n.equals(p.name())) {
                    current = p;
                }
            }

            finalRoster.add(current);
            uniquePlayers.remove(current);
        }

        return finalRoster.iterator();
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

        if (o instanceof Rosters) {

            Rosters that = (Rosters) o;

            for (Player p : that.players) {
                if (!(this.players.contains(p))) {
                    return false;
                }
            }

            for (Player p : this.players) {
                if (!(that.players.contains(p))) {
                    return false;
                }
            }

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

    public static void main (String[] args) {

        // PLAYERS for testing
        Player A = Players.make("Russell Wilson");
        Player A2 = Players.make("Russell Wilson");
        Player B = Players.make("Lamar Miller");
        Player C = Players.make("Tevin Coleman");
        Player D = Players.make("Julio Jones");
        Player E = Players.make("Dez Bryant");
        Player F = Players.make("Devin Funchess");
        Player G = Players.make("Greg Olsen");
        Player H = Players.make("Stephen Gostkowski");

        A2.changeInjuryStatus(true);
        E.changeSuspendedStatus(true);


        // ROSTERS for testing
        Roster Empty = Rosters.empty();
        Roster rA = Empty.with(A);
        Roster rAB = rA.with(B);
        Roster rABC = rAB.with(C);
        Roster rABCD = rABC.with(D);
        Roster rABCDE = rABCD.with(E);
        Roster rABCDEF = rABCDE.with(F);
        Roster rABCDEFG = rABCDEF.with(G);
        Roster rABCDEFGH = rABCDEFG.with(H);


        // ITERATORS for testing
        Iterator<Player> itp1 = rABCDEFGH.iterator();
        Iterator<Player> itp2 = rABCDEFGH.with(B).iterator();
        Iterator<Player> itpEmpty = Empty.iterator();


        // PLAYERS TESTS
        assert A.name().equals("Russell Wilson") :
                "PLAYERS TEST1:  Player name is Russell Wilson";
        assert A.available() :
                "PLAYERS TEST2:  Player A is available";
        assert !A2.available() :
                "PLAYERS TEST3:  Player A2 is not available from injury";
        assert !E.available() :
                "PLAYERS TEST4:  Player E is not available form suspension";
        assert A.underContract() :
                "PLAYERS TEST5:  Player A is under contract";
        assert !A.isInjured() :
                "PLAYERS TEST6:  Player A is not injured";
        assert A2.isInjured() :
                "PLAYERS TEST7:  Player A2 is injured";
        assert !A.isSuspended() :
                "PLAYERS TEST8:  Player A is not suspended";
        assert E.isSuspended() :
                "PLAYERS TEST9:  Player E is suspended";


        // ROSTERS TESTS
        assert rA.with(B).with(B).equals(rA.with(B)) :
                "ROSTERS TEST1:  Each player can only be on the roster once";
        assert Rosters.empty().without(B).equals(Rosters.empty()) :
                "ROSTERS TEST2:  Given player is not on the empty roster";
        assert rA.without(B).without(B).equals(rA.without(B)) :
                "ROSTERS TEST3:  Given player can only be removed once";
        assert !Rosters.empty().has(B) :
                "ROSTERS TEST4:  Player B is not on empty list";
        assert rA.with(B).has(B) :
                "ROSTERS TEST5:  Roster with player B contains player B";
        assert !rA.without(B).has(B) :
                "ROSTERS TEST6:  Roster without player B does not contain B";
        assert Rosters.empty().size() == 0 :
                "ROSTERS TEST7:  Empty roster has size 0";
        assert rA.without(B).size() == 1 :
                "ROSTERS TEST8:  Roster with one player has size 1";
        assert rA.with(B).size() == 2 :
                "ROSTERS TEST9:  Roster with two players has size 2";
        assert rA.with(B).with(B).size() == 2 :
                "ROSTERS TEST10: Roster with two players has size 2";
        assert rA.with(B).without(B).size() == 1 :
                "ROSTERS TEST11: Roster with one player has size 1";
        assert itp1.next().equals(itp2.next()) :
                "ROSTERS TEST12: Iterator should ignore duplicate player";
        assert itp1.hasNext() == itp2.hasNext() :
                "ROSTERS TEST13: Iterator should ignore duplicate player";
        assert !itpEmpty.hasNext() :
                "ROSTERS TEST14: Empty Iterator, should return false";
        assert rABCD.equals(rABCDE.readyRoster()) :
                "ROSTERS TEST15: Player E is not ready";
        assert rABCD.readyCount() == 4 :
                "ROSTERS TEST16: All four players are ready";
        assert rABCDE.readyCount() == 4 :
                "ROSTERS TEST17: Player E is not ready";

        System.out.println ("All unit tests of Competitor1 passed.");

    }
}
