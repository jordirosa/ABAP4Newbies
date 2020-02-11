*&---------------------------------------------------------------------*
*& Report YJRS_ABAP_0002
*&---------------------------------------------------------------------*
* This programs will show the problem of use a BINARY search on unsorted
* table.
REPORT YJRS_ABAP_0002.

START-OF-SELECTION.
  PERFORM start_of_selection.
*&---------------------------------------------------------------------*
*& Form START_OF_SELECTION
*&---------------------------------------------------------------------*
FORM start_of_selection .
  TYPES: BEGIN OF lt_unsorted,
           number TYPE i,
         END OF lt_unsorted.

  DATA: li_unsorted TYPE STANDARD TABLE OF lt_unsorted
                    WITH HEADER LINE.

  li_unsorted-number = 1.
  APPEND li_unsorted.
  li_unsorted-number = 3.
  APPEND li_unsorted.
  li_unsorted-number = 2.
  APPEND li_unsorted.

  READ TABLE li_unsorted WITH KEY number = 2
                         BINARY SEARCH.

  WRITE: / 'SY-SUBRC is:', sy-subrc.
ENDFORM.
