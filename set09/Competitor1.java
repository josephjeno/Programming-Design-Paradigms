// Constructor template for Competitor1:
//     new Competitor1 (Competitor c1)
//
// Interpretation: the competitor represents an individual or team

// Note:  In Java, you cannot assume a List is mutable, because all
// of the List operations that change the state of a List are optional.
// Mutation of a Java list is allowed only if a precondition or other
// invariant says the list is mutable and you are allowed to change it.

import java.util.*;

class Competitor1 implements Competitor {

    String name;              // the name of the competitor
    List<String> outranks;    // the names of the competitors that this
                              // competitor outranks
    List<String> outrankedBy; // the names of the competitors that outrank this
                              // competitor

    Competitor1 (String s) {
        this.name = s;
        this.outranks = new ArrayList<>();
        this.outrankedBy = new ArrayList<>();
    }

    // returns the name of this competitor
    public String name () {
        return this.name;
    }

    // GIVEN: another competitor and a list of outcomes
    // RETURNS: true iff one or more of the outcomes indicates this
    //     competitor has defeated or tied the given competitor

    public boolean hasDefeated (Competitor that, List<Outcome> outcomes) {

        boolean defeated = false;

        for (Outcome outcome: outcomes) {

            if (!defeated) {

                defeated = hasDefeatedOutcome(that, outcome);

            }
        }
        return defeated;
    }

    // GIVEN: a list of outcomes
    // RETURNS: a list of the names of all competitors mentioned by
    //     the outcomes that are outranked by this competitor,
    //     without duplicates, in alphabetical order

    public List<String> outranks (List<Outcome> outcomes) {

        List<String> list = new ArrayList<>();

        if (outcomes.size() != 0) {

            List<Competitor1> competitors = this.outcomeCycler(outcomes);

            list = competitors.get(
                    this.competitorPosition(competitors)).outranks();

        }

        return list;

    }

    // GIVEN: a list of outcomes
    // RETURNS: a list of the names of all competitors mentioned by
    //     the outcomes that outrank this competitor,
    //     without duplicates, in alphabetical order

    public List<String> outrankedBy (List<Outcome> outcomes) {

        List<String> list = new ArrayList<>();

        if (outcomes.size() != 0) {

            List<Competitor1> competitors = this.outcomeCycler(outcomes);

            list = competitors.get(
                    this.competitorPosition(competitors)).outrankedBy();

        }

        return list;

    }

    // GIVEN: a list of outcomes
    // RETURNS: a list of the names of all competitors mentioned by
    //     one or more of the outcomes, without repetitions, with
    //     the name of competitor A coming before the name of
    //     competitor B in the list if and only if the power-ranking
    //     of A is higher than the power ranking of B.

    public List<String> powerRanking (List<Outcome> outcomes) {

        List<String> powerList = new ArrayList<>();

        if (outcomes.size() != 0) {

            List<Competitor1> competitors = this.outcomeCycler(outcomes);

            while (competitors.size() > 0) {

                Competitor1 currentHighest = competitors.get(0);

                for (Competitor1 competitor : competitors) {

                    if (!currentHighest.isPowerRankingOutranks(
                            competitor, outcomes)) {

                        currentHighest = competitor;

                    }
                }

                competitors.remove(currentHighest);

                powerList.add(currentHighest.name());

            }

        }

        return powerList;

    }

    // GIVEN: another competitor, an outcome
    // RETURNS: true if this competitor defeated or tied the given competitor in
    //     the given outcome

    private boolean hasDefeatedOutcome (Competitor that, Outcome outcome) {

        boolean defeated;

        if (outcome.isTie()) {

            defeated = (this.name.equals(outcome.first().name()) &&
                    that.name().equals(outcome.second().name())) ||
                    (that.name().equals(outcome.first().name()) &&
                            this.name.equals(outcome.first().name()));

        } else {

            defeated = (this.name.equals(outcome.winner().name()) &&
                    that.name().equals(outcome.loser().name()));

        }

        return defeated;
    }

    // returns the outranks list of this competitor
    private List<String> outranks () {
        return this.outranks;
    }

    // returns the outrankedBy list of this competitor
    private List<String> outrankedBy () {
        return this.outrankedBy;
    }

    // GIVEN: a competitor that is outranked by this competitor
    // EFFECT: updates the outranks list of this competitor to include the given
    //     competitor

    private void insertOutranks (Competitor1 loser) {
        this.outranks.add(loser.name());
    }

    // GIVEN: a competitor that outranks this competitor
    // EFFECT: updates the outranked list of this competitor to include the
    //     given competitor

    private void insertOutrankedBy (Competitor1 winner) {
        this.outrankedBy.add(winner.name());
    }

    // GIVEN: a list of names
    // WHERE: this competitor outranks given list of names
    // EFFECT: updates the outranks list of this competitor to include the list
    //     of names, sorted by name, with no duplicates

    private void mergeOutranks (List<String> outranksList) {

        this.outranks.addAll(outranksList);

        this.outranks = new ArrayList<>(new LinkedHashSet<>(this.outranks));

        Collections.sort(this.outranks);

    }

    // EFFECT: sorts the outrankedBy list of this competitor and removes
    //     duplicates

    private void sortOutranked () {

        this.outrankedBy = new ArrayList<>(
                new LinkedHashSet<>(this.outrankedBy));

        Collections.sort(this.outrankedBy);
    }

    // GIVEN: a competitor and a list of outcomes
    // RETURNS: true if this competitor outranks the given competitor according
    //     to the powerRanking system

    private boolean isPowerRankingOutranks
            (Competitor1 that, List<Outcome> outcomes) {

        boolean outranks = false;

        if (this.outrankedBy().size() < that.outrankedBy().size()) {

            outranks = true;

        } else if (this.outrankedBy().size() > that.outrankedBy().size()) {

            outranks = false;

        } else if (this.outranks().size() > that.outranks().size()) {

            outranks = true;

        } else if (this.outranks().size() < that.outranks().size()) {

            outranks = false;

        } else if (this.nonLosingPercentage(outcomes) >
                that.nonLosingPercentage(outcomes)) {

            outranks = true;

        } else if (this.nonLosingPercentage(outcomes) <
                that.nonLosingPercentage(outcomes)) {

            outranks = false;

        } else if (this.name().compareTo(that.name()) < 0) {

            outranks = true;
        }

        return outranks;

    }

    // GIVEN: a list of outcomes
    // RETURNS: the non-losing-percentage of this competitor

    private float nonLosingPercentage (List<Outcome> outcomes) {

        float outcomesWon = 0;

        float outcomesMentioned = 0;

        for (Outcome outcome : outcomes) {

            if (outcome.isTie()) {

                if (this.name().equals(outcome.first().name()) ||
                        this.name().equals(outcome.second().name())) {

                    outcomesWon = outcomesWon + 1;

                    outcomesMentioned = outcomesMentioned + 1;

                }

            } else {

                if (this.name().equals(outcome.winner().name())) {

                    outcomesWon = outcomesWon + 1;

                    outcomesMentioned = outcomesMentioned + 1;

                } else if (this.name().equals(outcome.loser().name())) {

                    outcomesMentioned = outcomesMentioned + 1;

                }
            }
        }

        return outcomesWon / outcomesMentioned;

    }

    // GIVEN: a list of outcomes
    // RETURNS: the competitor list with all outcomes recorded

    private List<Competitor1> outcomeCycler (List<Outcome> outcomes) {

        List<Competitor1> competitors = new ArrayList<>();

        for (Outcome outcome : outcomes) {

            if (outcome.isTie()) {

                Competitor1 first = new Competitor1(outcome.first().name());
                Competitor1 second = new Competitor1(outcome.second().name());

                first.insertDefeat(second, competitors);
                second.insertDefeat(first, competitors);

            } else {

                Competitor1 winner = new Competitor1(outcome.winner().name());
                Competitor1 loser = new Competitor1(outcome.loser().name());

                winner.insertDefeat(loser, competitors);
            }
        }

        competitorHistoryCycler(competitors);

        return competitors;

    }

    // GIVEN: a list of competitors
    // WHERE: the list of outranks and outrankedBy have not been fully cycled
    // EFFECT: cycles the outranks and outrankedBy lists of all competitors,
    //     filling in competitor names when missing

    private void competitorHistoryCycler (List<Competitor1> competitors) {

        for (int a = 0; a < 5; a++) {

            for (Competitor1 comp : competitors) {

                comp.outranksCycler(competitors);

                comp.outrankedByCycler(competitors);

            }
        }
    }

    // GIVEN: a list of competitors
    // WHERE: this competitor is on the list of competitors
    // EFFECT: the outranks list of this competitor is updated

    private void outranksCycler (List<Competitor1> competitors) {

        for (int o = 0; o < this.outranks().size(); o++) {

            String loserName = this.outranks().get(o);

            Competitor1 loser = new Competitor1(loserName);

            List<String> loserOutranks =
                    competitors.get(
                            loser.competitorPosition(
                                    competitors)).outranks();

            this.mergeOutranks(loserOutranks);

        }
    }

    // GIVEN: a list of competitors
    // WHERE: this competitor is on the list of competitors
    // EFFECT: the outrankedBy list of this competitor is updated

    private void outrankedByCycler (List<Competitor1> competitors) {

        for (Competitor1 that: competitors) {

            if (that.outranks().contains(this.name())) {

                this.insertOutrankedBy(that);

            }
        }

        this.sortOutranked();
    }

    // GIVEN: a list of competitors
    // RETURNS: true iff the name of this competitor is equal to a name of a
    //     competitor on the list of competitors

    private boolean containsCompetitor (List<Competitor1> competitors) {

        boolean contains = false;

        for (Competitor1 competitor : competitors) {
            if (!contains) {
                contains = this.name.equals(competitor.name());
            }
        }

        return contains;

    }

    // GIVEN: a list of competitors
    // WHERE: this competitor is on the list of competitors
    // RETURNS: the location of this competitor on the list of competitors

    private int competitorPosition (List<Competitor1> competitors) {

        int location = -1;

        for (int i = 0; i < competitors.size(); i++) {
            if (this.name.equals(competitors.get(i).name())) {
                location = i;
            }
        }

        return location;
    }

    // GIVEN: a competitor and a list of competitors
    // WHERE: this competitor has defeated the given competitor
    // EFFECT: records the defeat on the list of competitors

    private void insertDefeat
            (Competitor1 loser, List<Competitor1> competitors) {

        if (this.containsCompetitor(competitors)) {

            int WinnerPosition = this.competitorPosition(competitors);
            Competitor1 winner = competitors.get(WinnerPosition);
            winner.insertOutranks(loser);

        } else {

            competitors.add(this);
            this.insertOutranks(loser);

        }

        if (loser.containsCompetitor(competitors)) {

            int LoserPosition = loser.competitorPosition(competitors);
            Competitor1 losingComp = competitors.get(LoserPosition);
            losingComp.insertOutrankedBy(this);

        } else {

            competitors.add(loser);
            loser.insertOutrankedBy(this);

        }
    }

    public static void main (String[] args) {

        // COMPETITORS for testing
        Competitor A = new Competitor1 ("A");
        Competitor B = new Competitor1 ("B");
        Competitor C = new Competitor1 ("C");
        Competitor D = new Competitor1 ("D");
        Competitor E = new Competitor1 ("E");
        Competitor F = new Competitor1 ("F");
        Competitor G = new Competitor1 ("G");
        Competitor H = new Competitor1 ("H");
        Competitor I = new Competitor1 ("I");
        Competitor J = new Competitor1 ("J");
        Competitor K = new Competitor1 ("K");
        Competitor L = new Competitor1 ("L");
        Competitor M = new Competitor1 ("M");
        Competitor N = new Competitor1 ("N");
        Competitor O = new Competitor1 ("O");
        Competitor P = new Competitor1 ("P");
        Competitor Q = new Competitor1 ("Q");
        Competitor R = new Competitor1 ("R");
        Competitor S = new Competitor1 ("S");
        Competitor T = new Competitor1 ("T");
        Competitor U = new Competitor1 ("U");
        Competitor V = new Competitor1 ("V");
        Competitor W = new Competitor1 ("W");
        Competitor X = new Competitor1 ("X");
        Competitor Y = new Competitor1 ("Y");
        Competitor Z = new Competitor1 ("Z");

        // OUTCOMES for testing
        Outcome DEF_AB = new Defeat1(A, B);
        Outcome DEF_AC = new Defeat1(A, C);
        Outcome DEF_AD = new Defeat1(A, D);
        Outcome DEF_AE = new Defeat1(A, E);
        Outcome DEF_BA = new Defeat1(B, A);
        Outcome DEF_BC = new Defeat1(B, C);
        Outcome DEF_CA = new Defeat1(C, A);
        Outcome DEF_CB = new Defeat1(C, B);
        Outcome DEF_CD = new Defeat1(C, D);
        Outcome DEF_CE = new Defeat1(C, E);
        Outcome DEF_CF = new Defeat1(C, F);
        Outcome DEF_DC = new Defeat1(D, C);
        Outcome DEF_DE = new Defeat1(D, E);
        Outcome DEF_EH = new Defeat1(E, H);
        Outcome DEF_FE = new Defeat1(F, E);
        Outcome DEF_FI = new Defeat1(F, I);
        Outcome DEF_GK = new Defeat1(G, K);
        Outcome DEF_HL = new Defeat1(H, L);
        Outcome DEF_IM = new Defeat1(I, M);
        Outcome DEF_JN = new Defeat1(J, N);
        Outcome DEF_JP = new Defeat1(J, P);
        Outcome DEF_KO = new Defeat1(K, O);
        Outcome DEF_LP = new Defeat1(L, P);
        Outcome DEF_MK = new Defeat1(M, K);
        Outcome DEF_NL = new Defeat1(N, L);
        Outcome DEF_OA = new Defeat1(O, A);
        Outcome DEF_PB = new Defeat1(P, B);
        Outcome DEF_RK = new Defeat1(R, K);
        Outcome DEF_TA = new Defeat1(T, A);
        Outcome DEF_UB = new Defeat1(U, B);
        Outcome DEF_VE = new Defeat1(V, E);
        Outcome DEF_WP = new Defeat1(W, P);
        Outcome DEF_YE = new Defeat1(Y, E);
        Outcome DEF_ZP = new Defeat1(Z, P);

        Outcome TIE_AB = new Tie1(A, B);
        Outcome TIE_AC = new Tie1(A, C);
        Outcome TIE_AE = new Tie1(A, E);
        Outcome TIE_BC = new Tie1(B, C);
        Outcome TIE_BD = new Tie1(B, D);
        Outcome TIE_CD = new Tie1(C, D);
        Outcome TIE_CE = new Tie1(C, E);
        Outcome TIE_DB = new Tie1(D, B);
        Outcome TIE_DE = new Tie1(D, E);
        Outcome TIE_EH = new Tie1(E, H);
        Outcome TIE_FI = new Tie1(F, I);
        Outcome TIE_GK = new Tie1(G, K);
        Outcome TIE_HL = new Tie1(H, L);
        Outcome TIE_IM = new Tie1(I, M);
        Outcome TIE_JN = new Tie1(J, N);
        Outcome TIE_JP = new Tie1(J, P);
        Outcome TIE_KO = new Tie1(K, O);
        Outcome TIE_LP = new Tie1(L, P);
        Outcome TIE_MK = new Tie1(M, K);
        Outcome TIE_NL = new Tie1(N, L);
        Outcome TIE_PB = new Tie1(P, B);
        Outcome TIE_QP = new Tie1(Q, P);
        Outcome TIE_SL = new Tie1(S, L);
        Outcome TIE_UB = new Tie1(U, B);
        Outcome TIE_VE = new Tie1(V, E);
        Outcome TIE_XB = new Tie1(X, B);
        Outcome TIE_ZP = new Tie1(Z, P);

        // LIST<Outcome> for testing
        List<Outcome> BLANK = new ArrayList<>();
        List<Outcome> TEST1 =
                new ArrayList<>(Arrays.asList(TIE_AB, DEF_BA, TIE_BC));

        List<Outcome> TEST2 =
                new ArrayList<>(Arrays.asList(
                        TIE_AE, DEF_CB, DEF_BA, DEF_AC, TIE_BC));

        List<Outcome> TEST3 =
                new ArrayList<>(Arrays.asList(
                        DEF_CB, DEF_BA, DEF_AC, TIE_BC));

        List<Outcome> TEST4 =
                new ArrayList<>(Arrays.asList(
                        DEF_CB, DEF_AC, TIE_BD));

        List<Outcome> TEST5 =
                new ArrayList<>(Arrays.asList(
                        DEF_CE, DEF_BC, TIE_BD, TIE_DB));

        List<Outcome> TEST6 =
                new ArrayList<>(Arrays.asList(
                        DEF_AB, DEF_BC, DEF_CD, DEF_DE, TIE_AE));

        List<Outcome> TEST7 =
                new ArrayList<>(Arrays.asList(
                        DEF_AB, DEF_BC, DEF_CD, DEF_DE, TIE_CE));

        List<Outcome> TEST9 =
                new ArrayList<>(Arrays.asList(
                        DEF_AB, DEF_BC, DEF_CD, DEF_DE, DEF_EH, DEF_FI, DEF_GK,
                        DEF_HL, DEF_IM, DEF_JN, DEF_KO, DEF_LP, DEF_MK, DEF_NL,
                        DEF_OA, DEF_PB, TIE_CE));

        List<Outcome> TEST11 =
                new ArrayList<>(Arrays.asList(
                        DEF_AB, DEF_BC, DEF_CD, DEF_DE, DEF_EH, DEF_FI, DEF_GK,
                        DEF_HL, DEF_IM, DEF_JN, DEF_KO, DEF_LP, DEF_MK, DEF_NL,
                        DEF_OA, DEF_PB, TIE_CE, TIE_JP));

        List<Outcome> TEST13 =
                new ArrayList<>(Arrays.asList(
                        DEF_AB, TIE_BC, DEF_CD, TIE_DE, DEF_EH, TIE_FI, TIE_GK,
                        DEF_HL, DEF_IM, TIE_JN, DEF_KO, TIE_LP, DEF_MK, TIE_NL,
                        DEF_OA, TIE_PB, TIE_CE, TIE_JP));

        List<Outcome> TEST15 =
                new ArrayList<>(Arrays.asList(
                        DEF_AB, DEF_BA, TIE_BC));

        List<Outcome> TEST17 =
                new ArrayList<>(Arrays.asList(
                        TIE_AB, DEF_BA, TIE_BC));

        List<Outcome> TEST18 =
                new ArrayList<>(Arrays.asList(
                        DEF_BC, DEF_CB, TIE_AB, TIE_AC, DEF_CA));

        List<Outcome> TEST19 =
                new ArrayList<>(Arrays.asList(
                        DEF_AB, DEF_BC, DEF_CD, TIE_DE, DEF_EH, TIE_FI, TIE_GK,
                        DEF_HL, DEF_IM, TIE_JN, TIE_PB, TIE_CE, DEF_JP, TIE_QP,
                        DEF_RK, TIE_SL, DEF_TA, DEF_UB, DEF_VE, DEF_WP, TIE_XB,
                        DEF_YE, DEF_ZP));

        List<Outcome> TEST20 =
                new ArrayList<>(Arrays.asList(
                        DEF_AB, TIE_BC, DEF_CD, TIE_DE, DEF_EH, TIE_FI, DEF_RK,
                        TIE_SL, DEF_TA, TIE_UB, TIE_VE, DEF_WP, TIE_XB,
                        DEF_YE, TIE_ZP));

        List<Outcome> TEST21 =
                new ArrayList<>(Arrays.asList(
                        DEF_AB, TIE_BC, DEF_CD, TIE_DE, DEF_EH, TIE_FI, TIE_GK,
                        DEF_HL, DEF_IM, TIE_JN, DEF_KO, TIE_LP, DEF_MK, TIE_NL,
                        DEF_OA, TIE_PB, TIE_CE, DEF_JP, TIE_QP, DEF_RK, TIE_SL,
                        DEF_TA, TIE_UB, TIE_VE, DEF_WP, TIE_XB, DEF_YE, TIE_ZP)
                );

        List<Outcome> TEST22 =
                new ArrayList<>(Arrays.asList(
                        TIE_BC, TIE_AB));

        List<Outcome> TEST23 =
                new ArrayList<>(Arrays.asList(
                        DEF_AB, TIE_BC, TIE_DE, DEF_EH, TIE_FI, DEF_HL, DEF_IM,
                        TIE_JN, DEF_KO, TIE_LP, DEF_MK, TIE_PB, TIE_CE, TIE_JP)
                );

        List<Outcome> TEST24 =
                new ArrayList<>(Arrays.asList(
                        TIE_AB, TIE_BC, TIE_CD, TIE_DE, TIE_EH, TIE_FI, TIE_GK,
                        TIE_HL, TIE_IM, TIE_JN, TIE_KO, TIE_LP, TIE_MK, TIE_NL,
                        DEF_OA, TIE_PB, TIE_CE, TIE_JP));

        List<Outcome> TEST25 =
                new ArrayList<>(Arrays.asList(
                        TIE_AE, DEF_CB, DEF_BA, DEF_AC, TIE_BC));

        List<Outcome> TEST26 =
                new ArrayList<>(Arrays.asList(
                        DEF_CE, DEF_DC, TIE_DB));

        List<Outcome> TEST27 =
                new ArrayList<>(Arrays.asList(
                        DEF_AB, DEF_BC, DEF_CD, DEF_DE, TIE_CE));

        List<Outcome> TEST28 =
                new ArrayList<>(Arrays.asList(
                        DEF_AB, DEF_BC, DEF_CD, DEF_DE, DEF_EH, DEF_FI, DEF_IM,
                        DEF_MK, DEF_NL, DEF_OA, DEF_PB, TIE_CE));

        List<Outcome> TEST29 =
                new ArrayList<>(Arrays.asList(
                        DEF_AB, DEF_BC, DEF_CD, DEF_DE, DEF_EH, DEF_FI, DEF_GK,
                        DEF_HL, DEF_IM, DEF_JN, DEF_KO, DEF_LP, DEF_MK, DEF_NL,
                        DEF_OA, DEF_PB, TIE_CE));

        List<Outcome> TEST30 =
                new ArrayList<>(Arrays.asList(
                        DEF_AB, DEF_BC, DEF_CD, DEF_DE, DEF_EH, DEF_FI, DEF_GK,
                        DEF_HL, DEF_IM, DEF_JN, DEF_KO, DEF_LP, DEF_MK, DEF_NL,
                        DEF_OA, DEF_PB, TIE_CE, TIE_JP));

        List<Outcome> TEST31 =
                new ArrayList<>(Arrays.asList(
                        DEF_AD, DEF_AE, DEF_CB, DEF_CF, TIE_DB, DEF_FE));

        List<Outcome> TEST32 =
                new ArrayList<>(Arrays.asList(
                        DEF_AB, TIE_BC, DEF_CD, TIE_DE, DEF_EH, TIE_FI, TIE_GK,
                        DEF_HL, DEF_IM, TIE_JN, DEF_KO, TIE_LP, DEF_MK, TIE_NL,
                        DEF_OA, TIE_PB, TIE_CE, TIE_JP));


        // TESTS
        assert A.hasDefeated(B, TEST1) == true :
                "TEST1: Should return True since there is tie between A and B";
        assert A.hasDefeated(B, TEST2) == false :
                "TEST2: Should return false since defeat is not transitive";
        assert A.outranks(TEST3).equals(
                new ArrayList<>(Arrays.asList("A", "B", "C"))) :
                "TEST3: There exists a cyclic path between the competitors";
        assert A.outranks(TEST4).equals(
                new ArrayList<>(Arrays.asList("B", "C", "D"))) :
                "TEST4: A outranks B and D transitively";
        assert E.outranks(TEST5).equals(
                new ArrayList<String>()) :
                "TEST5: E does not outrank anyone, hence list should be empty";
        assert A.outranks(TEST6).equals(
                new ArrayList<>(Arrays.asList("A", "B", "C", "D", "E"))) :
                "TEST6: There exists a cyclic path between the competitors";
        assert C.outrankedBy(TEST7).equals(
                new ArrayList<>(Arrays.asList("A", "B", "C", "D", "E"))) :
                "TEST7: There exists a cyclic path between C and E";
        assert A.outrankedBy(TEST7).equals(
                new ArrayList<String>()) :
                "TEST8: A never gets defeated or tied, hence list should be " +
                        "empty";
        assert F.outrankedBy(TEST9).equals(
                new ArrayList<String>()) :
                "TEST9: F never gets defeated or tied, hence list should be " +
                        "empty";
        assert F.outranks(TEST9).equals(
                new ArrayList<>(Arrays.asList("A", "B", "C", "D", "E", "H", "I",
                        "K", "L", "M", "O", "P"))) :
                "TEST10: There exists multiple cyclic paths between the " +
                        "competitors";
        assert E.outrankedBy(TEST11).equals(
                new ArrayList<>(Arrays.asList("A", "B", "C", "D", "E", "F", "G",
                        "H", "I", "J", "K", "L", "M", "N", "O", "P"))) :
                "TEST11: There exists multiple cyclic paths between the " +
                        "competitors";
        assert E.outranks(TEST11).equals(
                new ArrayList<>(Arrays.asList("B", "C", "D", "E", "H", "J", "L",
                        "N", "P"))) :
                "TEST12: There exists multiple cyclic paths between the " +
                        "competitors";
        assert F.outrankedBy(TEST13).equals(
                new ArrayList<>(Arrays.asList("F", "I"))) :
                "TEST13: F is outranked by F because it ties with I";
        assert F.outranks(TEST13).equals(
                new ArrayList<>(Arrays.asList("A", "B", "C", "D", "E", "F", "G",
                        "H", "I", "J", "K", "L", "M", "N", "O", "P"))) :
                "TEST14: There exists multiple cyclic paths between the " +
                        "competitors";
        assert A.hasDefeated(B, TEST15) == true :
                "TEST15: Should return true";
        assert E.outranks(TEST13).equals(
                new ArrayList<>(Arrays.asList("B", "C", "D", "E", "H", "J", "L",
                        "N", "P"))) :
                "TEST16: There exists multiple cyclic paths between the " +
                        "competitors";
        assert A.hasDefeated(C, TEST17) == false :
                "TEST17: Should return false since defeat is not transitive";
        assert A.powerRanking(TEST18).equals(
                new ArrayList<>(Arrays.asList("C", "A", "B"))) :
                "TEST18: C has higher non losing percentage than A";
        assert A.powerRanking(TEST19).equals(
                new ArrayList<>(Arrays.asList("T", "U", "W", "Z", "V", "Y", "R",
                        "A", "J", "N", "F", "I", "M", "G", "K", "Q", "X", "B",
                        "P", "C", "E", "D", "H", "S", "L"))) :
                "TEST19: There exists multiple cyclic paths between the " +
                        "competitors";
        assert A.powerRanking(TEST20).equals(
                new ArrayList<>(Arrays.asList("T", "Y", "W", "R", "A", "K", "F",
                        "I", "L", "S", "Z", "P", "C", "U", "X", "B", "V", "E",
                        "D", "H"))) :
                "TEST20: There exists multiple cyclic paths between the " +
                        "competitors";
        assert A.powerRanking(TEST21).equals(
                new ArrayList<>(Arrays.asList("R", "T", "W", "Y", "F", "I", "M",
                        "G", "K", "O", "A", "C", "J", "N", "Q", "S", "U", "V",
                        "X", "Z", "B", "E", "L", "P", "D", "H"))) :
                "TEST21: There exists multiple cyclic paths between the " +
                        "competitors";
        assert A.powerRanking(TEST22).equals(
                new ArrayList<>(Arrays.asList("A", "B", "C"))) :
                "TEST22: Alphabetical order gets the precedence";
        assert A.powerRanking(TEST23).equals(
                new ArrayList<>(Arrays.asList("A", "F", "I", "M", "K", "O", "C",
                        "D", "E", "J", "N", "P", "B", "H", "L"))) :
                "TEST23: There exists multiple cyclic paths between the " +
                        "competitors";
        assert A.powerRanking(TEST24).equals(
                new ArrayList<>(Arrays.asList("F", "G", "I", "K", "M", "O", "B",
                        "C", "D", "E", "H", "J", "L", "N", "P", "A"))) :
                "TEST24: There exists multiple cyclic paths between the " +
                        "competitors";
        assert A.powerRanking(TEST25).equals(
                new ArrayList<>(Arrays.asList("E", "A", "B", "C"))) :
                "TEST25: E has the highest  non-losing percentage";
        assert A.powerRanking(TEST26).equals(
                new ArrayList<>(Arrays.asList("B", "D", "C", "E"))) :
                "TEST26: Alphabetical order gets the precedence";
        assert A.powerRanking(TEST27).equals(
                new ArrayList<>(Arrays.asList("A", "B", "C", "D", "E"))) :
                "TEST27: Alphabetical order gets the precedence";
        assert A.powerRanking(TEST28).equals(
                new ArrayList<>(Arrays.asList("O", "P", "F", "N", "A", "I", "L",
                        "M", "B", "K", "C", "E", "D", "H"))) :
                "TEST28: There exists multiple cyclic paths between the " +
                        "competitors";
        assert A.powerRanking(TEST29).equals(
                new ArrayList<>(Arrays.asList("F", "G", "J", "I", "N", "M", "K",
                        "O", "A", "C", "E", "D", "H", "P", "B", "L"))) :
                "TEST29: There exists multiple cyclic paths between the " +
                        "competitors";
        assert A.powerRanking(TEST30).equals(
                new ArrayList<>(Arrays.asList("F", "G", "I", "M", "K", "O", "A",
                        "J", "C", "E", "P", "D", "H", "N", "B", "L"))) :
                "TEST30: There exists multiple cyclic paths between the " +
                        "competitors";
        assert A.powerRanking(TEST31).equals(
                new ArrayList<>(Arrays.asList("C", "A", "F", "E", "B", "D"))) :
                "TEST31: C outranks 4";
        assert A.powerRanking(TEST32).equals(
                new ArrayList<>(Arrays.asList("F", "I", "M", "G", "K", "O", "A",
                        "C", "E", "J", "N", "P", "B", "L", "D", "H"))) :
                "TEST32: There exists multiple cyclic paths between the " +
                        "competitors";
        assert A.hasDefeated(B, BLANK) == false :
                "TEST33: Blank Outcome list given";
        assert A.outranks(BLANK).equals(new ArrayList<>()) :
                "TEST34: Blank Outcome list given";
        assert A.outrankedBy(BLANK).equals(new ArrayList<>()) :
                "TEST35: Blank Outcome list given";
        assert A.powerRanking(BLANK).equals(new ArrayList<>()) :
                "TEST36: Blank Outcome list given";


        System.out.println ("All unit tests of Competitor1 passed.");

    }
}
