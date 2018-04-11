import java.util.stream.Collectors;
import java.util.*;

public class TestRosterWithStream {

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
        RosterWithStream Empty = RosterWithStreams.empty();
        RosterWithStream rA = Empty.with(A);
        RosterWithStream rAB = rA.with(B);
        RosterWithStream rABC = rAB.with(C);
        RosterWithStream rABCD = rABC.with(D);
        RosterWithStream rABCDE = rABCD.with(E);
        RosterWithStream rABCDEF = rABCDE.with(F);
        RosterWithStream rABCDEFG = rABCDEF.with(G);
        RosterWithStream rABCDEFGH = rABCDEFG.with(H);


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
        assert RosterWithStreams.empty().without(B).equals(RosterWithStreams.empty()) :
                "ROSTERS TEST2:  Given player is not on the empty roster";
        assert rA.without(B).without(B).equals(rA.without(B)) :
                "ROSTERS TEST3:  Given player can only be removed once";
        assert !RosterWithStreams.empty().has(B) :
                "ROSTERS TEST4:  Player B is not on empty list";
        assert rA.with(B).has(B) :
                "ROSTERS TEST5:  Roster with player B contains player B";
        assert !rA.without(B).has(B) :
                "ROSTERS TEST6:  Roster without player B does not contain B";
        assert RosterWithStreams.empty().size() == 0 :
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


        // STREAM TESTS

        // allMatch
        assert Empty.stream().allMatch(x -> Objects.equals(x, A)) :
                "allMatch TEST1:  Empty stream should return true";
        assert rA.stream().allMatch(x -> Objects.equals(x, A)) :
                "allMatch TEST2:  Stream with only A should return true";
        assert !rAB.stream().allMatch(x -> Objects.equals(x, A)) :
                "allMatch TEST3:  Stream with multiple players returns false";

        // anyMatch
        assert !Empty.stream().anyMatch(x -> Objects.equals(x, A)) :
                "anyMatch TEST1:  Empty stream should return false";
        assert rABCD.stream().anyMatch(x -> Objects.equals(x, A)) :
                "anyMatch TEST2:  Should return true as stream contains A";
        assert !rABCD.without(A).stream().anyMatch(x -> Objects.equals(x, A)) :
                "anyMatch TEST3:  Should return false as there is no A";

        // count
        assert Empty.stream().count() == 0 :
                "count TEST1:  Empty stream has a count of 0";
        assert rABCD.stream().count() == 4 :
                "count TEST2:  Stream has 4 players";
        assert rABCD.with(D).stream().count() == 4 :
                "count TEST3:  Only 4 players as D is a duplicate player";

        // distinct
        assert Empty.stream().distinct().count() == Empty.stream().count() :
                "distinct TEST1:  Streams have equal count as already distinct";
        assert rABCD.stream().distinct().count() == rABCD.stream().count() :
                "distinct TEST2:  Streams have equal count as already distinct";
        assert rABCD.with(D).stream().distinct().count() ==
                rABCD.with(D).stream().count() :
                "distinct TEST3:  Streams have equal count as already distinct";

        // findAny
        assert !Empty.stream().findAny().isPresent() :
                "findAny TEST1:  Empty stream has no players";
        assert rA.stream().findAny().isPresent() :
                "findAny TEST2:  Player A is present";
        assert rA.with(A).stream().findAny().isPresent() :
                "findAny TEST3:  Player A is present";

        // findFirst
        assert !Empty.stream().findFirst().isPresent() :
                "findFirst TEST1:  Empty stream has no players";
        assert rA.stream().findFirst().isPresent() :
                "findFirst TEST2:  Player A is present";
        assert rA.with(A).stream().findFirst().isPresent() :
                "findFirst TEST3:  Player A is present";

        // forEach
        List<Player> ListForEach = new ArrayList<>();
        rABCD.stream().forEach(ListForEach::add);

        List<Player> ListManual = new ArrayList<>();
        ListManual.add(A);
        ListManual.add(B);
        ListManual.add(C);
        ListManual.add(D);

        assert ListForEach.equals(ListManual) :
                "forEach TEST1:  Both lists should be equal";

        // map
        List<String> EmptyName =
                Empty.stream().map(Player::name).collect(Collectors.toList());
        List<String> rAName =
                rA.stream().map(Player::name).collect(Collectors.toList());
        List<String> rABCName =
                rABC.stream().map(Player::name).collect(Collectors.toList());

        assert EmptyName.equals(new ArrayList<>()) :
                "map TEST1:  List is empty";
        assert rAName.equals(new ArrayList<>(Arrays.asList(A.name()))) :
                "map TEST2:  List should contain A's name";
        assert rABCName.equals(
                new ArrayList<>(Arrays.asList(A.name(), B.name(), C.name()))) :
                "map TEST3:  List should contain A, B, and C's name";

        // reduce
        assert Empty.stream()
                .map(Player::hashCode)
                .reduce(10,(x,y)-> x + y) == Empty.hashCode() + 10 :
                "reduce TEST1:  Should equal Roster Empty hashCode + 10";
        assert rABCD.stream()
                .map(Player::hashCode)
                .reduce(10,(x,y)-> x + y) == rABCD.hashCode() + 10 :
                "reduce TEST2:  Should equal Roster rABCD hashCode + 10";

        // skip
        assert rA.stream().skip(1).count() == Empty.stream().count() :
                "skip TEST1:  Should equal empty roster";
        assert rAB.stream().skip(1).count() == rA.stream().count() :
                "skip TEST2:  Should equal roster with one player ";

        // toArray
        assert rABC.stream().map(Player::name).toArray(String[]::new).length == 3 :
                "toArray TEST1:  Should be of length 3";
        assert Empty.stream().map(Player::name).toArray(String[]::new).length == 0 :
                "toArray TEST2:  Should be of length 0";

        System.out.println ("All unit tests of Competitor1 passed.");

    }
}
