package com.anvil.calculator;

import org.springframework.stereotype.Service;

/**
 * Calculator Service - Migrated from COBOL CALCULATOR module
 *
 * This class preserves the exact business logic from the legacy COBOL system.
 * Source: calculator.cbl
 */
@Service
public class Calculator {

    // COBOL Working Storage Section - Data fields
    // WS-NUM1: PIC 9(5) -> int (first operand)
    private int wsNum1 = 0;

    // WS-NUM2: PIC 9(5) -> int (second operand)
    private int wsNum2 = 0;

    // WS-RESULT: PIC 9(10) -> long (operation result)
    private long wsResult = 0L;

    // WS-OPERATION: PIC X(1) -> String (operation type)
    private String wsOperation = " ";

    /**
     * MAIN-PARAGRAPH - Entry point matching COBOL procedure
     * Initializes operands and performs all arithmetic operations
     */
    public void mainParagraph() {
        // MOVE 100 TO WS-NUM1
        wsNum1 = 100;

        // MOVE 50 TO WS-NUM2
        wsNum2 = 50;

        // PERFORM ADD-NUMBERS
        addNumbers();

        // PERFORM SUBTRACT-NUMBERS
        subtractNumbers();

        // PERFORM MULTIPLY-NUMBERS
        multiplyNumbers();

        // STOP RUN - handled by method return
    }

    /**
     * ADD-NUMBERS - Addition operation
     * Implements: ADD WS-NUM1 TO WS-NUM2 GIVING WS-RESULT
     */
    private void addNumbers() {
        // ADD operation: operand1 + operand2 -> result
        wsResult = wsNum1 + wsNum2;

        // DISPLAY "SUM: " WS-RESULT
        System.out.println("SUM: " + wsResult);
    }

    /**
     * SUBTRACT-NUMBERS - Subtraction operation
     * Implements: SUBTRACT WS-NUM2 FROM WS-NUM1 GIVING WS-RESULT
     */
    private void subtractNumbers() {
        // SUBTRACT operation: operand1 - operand2 -> result
        wsResult = wsNum1 - wsNum2;

        // DISPLAY "DIFFERENCE: " WS-RESULT
        System.out.println("DIFFERENCE: " + wsResult);
    }

    /**
     * MULTIPLY-NUMBERS - Multiplication operation
     * Implements: MULTIPLY WS-NUM1 BY WS-NUM2 GIVING WS-RESULT
     */
    private void multiplyNumbers() {
        // MULTIPLY operation: operand1 * operand2 -> result
        wsResult = wsNum1 * wsNum2;

        // DISPLAY "PRODUCT: " WS-RESULT
        System.out.println("PRODUCT: " + wsResult);
    }

    /**
     * Main method for standalone execution
     * Demonstrates the Calculator service functionality
     */
    public static void main(String[] args) {
        Calculator calculator = new Calculator();
        calculator.mainParagraph();
    }
}
