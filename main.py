# -*- coding: utf-8 -*-
"""
Created on Sat Mar 27 18:37:32 2021

@author: rorod
"""
import pygame
import random
import sys
#constants
BOARD_SIZE=483
WIDTH,HEIGHT=780, 483


FPS = 60
WIN = pygame.display.set_mode((WIDTH,HEIGHT))
WIN.fill((255,255,255))
pygame.display.set_caption("Othello")

def getRowCol(x,y):
    row = x//60
    col = y//60
    return row,col

def drawChip(row,col, fill,line):
    x=3+row*60+29
    y=3+col*60+29
    pygame.draw.circle(WIN,fill,(x,y),21,0)
    pygame.draw.circle(WIN,line,(x,y),22,3)

def drawEmptyCanvas():
    area = pygame.Rect(0,0,BOARD_SIZE,BOARD_SIZE)
    pygame.draw.rect(WIN,(0,0,0),area,0)
    for i in range(8):
        for j in range(8):
            rect =pygame.Rect(3+60*(i),3+60*(j),57,57)
            pygame.draw.rect(WIN,(38,148,38),rect,0)
            
     
    #OTHELLO implementation
def drawBoard(board):
    #
       # This function prints out the board that it was passed. Returns None.
    fill=(38,148,38)
    line = (38,148,38)
    for y in range(8):        
        for x in range(8):
            tile = board[x][y]
            line = (10,10,10)
            if(tile == 'O'):
                fill = (240,240,240)
            elif(tile == 'X'):
                fill=(25,25,25)
            else:
                continue
            drawChip(x,y,fill,line)
            
def resetBoard(board):
      # Blanks out the board it is passed, except for the original starting position.
    drawEmptyCanvas()
    
    for x in range(8):
        for y in range(8):
            board[x][y] = ' '
      # Starting pieces:
    board[3][3] = 'X'
    board[3][4] = 'O'
    board[4][3] = 'O'
    board[4][4] = 'X'
     
def getNewBoard():
      # Creates a brand new, blank board data structure.
    board = []
    for i in range(8):
        board.append([' '] * 8)
    return board

def isOnBoard(x, y):
      # Returns True if the coordinates are located on the board.
    return x >= 0 and x <= 7 and y >= 0 and y <=7

def isValidMove(board, tile, xstart, ystart):
      # Returns False if the player's move on space xstart, ystart is invalid.
      # If it is a valid move, returns a list of spaces that would become the player's if they made a move here.
    if board[xstart][ystart] != ' ' or not isOnBoard(xstart, ystart):
        return False
    board[xstart][ystart] = tile # temporarily set the tile on the board.
    if tile == 'X':
        otherTile = 'O'
    else:
        otherTile = 'X'
    tilesToFlip = []
    for xdirection, ydirection in [[0, 1], [1, 1], [1, 0], [1, -1], [0, -1], [-1, -1], [-1, 0], [-1, 1]]:
        x, y = xstart, ystart
        x += xdirection # first step in the direction
        y += ydirection # first step in the direction
        if isOnBoard(x, y) and board[x][y] == otherTile:
               # There is a piece belonging to the other player next to our piece.
            x += xdirection
            y += ydirection
            if not isOnBoard(x, y):
                continue
            while board[x][y] == otherTile:
                x += xdirection
                y += ydirection
                if not isOnBoard(x, y): # break out of while loop, then continue in for loop
                    break
            if not isOnBoard(x, y):
                continue
            if board[x][y] == tile:
                  # There are pieces to flip over. Go in the reverse direction until we reach the original space, noting all the tiles along the way.
                while True:
                    x -= xdirection
                    y -= ydirection
                    if x == xstart and y == ystart:
                        break
                    tilesToFlip.append([x, y])
    board[xstart][ystart] = ' ' # restore the empty space
    if len(tilesToFlip) == 0: # If no tiles were flipped, this is not a valid move.
        return False
    return tilesToFlip       

def getValidMoves(board, tile):
    #
     # Returns a list of [x,y] lists of valid moves for the given player on the given board.
    validMoves = []
    for x in range(8):
       for y in range(8):
           if isValidMove(board, tile, x, y) != False:
               validMoves.append([x, y])
    return validMoves

def getBoardWithValidMoves(board, tile):
    #
      # Returns a new board with . marking the valid moves the given player can make.
    dupeBoard = getBoardCopy(board)
    for x, y in getValidMoves(dupeBoard, tile):
      dupeBoard[x][y] = '.'
    return dupeBoard

def getBoardCopy(board):
     # Make a duplicate of the board list and return the duplicate.
  dupeBoard = getNewBoard()
  for x in range(8):
      for y in range(8):
          dupeBoard[x][y] = board[x][y]
  return dupeBoard  

def getScoreOfBoard(board):
     # Determine the score by counting the tiles. Returns a dictionary with keys 'X' and 'O'.
    xscore = 0
    oscore = 0
    for x in range(8):
        for y in range(8):
            if board[x][y] == 'X':
                xscore += 1
            if board[x][y] == 'O':
                oscore += 1
    return {'X':xscore, 'O':oscore};

#REWORK!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
def enterPlayerTile():
     # Lets the player type which tile they want to be.
     # Returns a list with the player's tile as the first item, and the computer's tile as the second.
  tile = ''
  while not (tile == 'X' or tile == 'O'):
      print('Do you want to be X or O?')
      tile = input().upper()
     # the first element in the list is the player's tile, the second is the computer's tile.
  if tile == 'X':
      return ['X', 'O']
  else:
      return ['O', 'X']      
  
def whoGoesFirst():
     # Randomly choose the player who goes first.
  if random.randint(0, 1) == 0:
      return 'computer'
  else:
      return 'player'
def playAgain():
     # This function returns True if the player wants to play again, otherwise it returns False.
  print('Do you want to play again? (yes or no)')
  return input().lower().startswith('y')
def makeMove(board, tile, xstart, ystart):
     # Place the tile on the board at xstart, ystart, and flip any of the opponent's pieces.
     # Returns False if this is an invalid move, True if it is valid.
  tilesToFlip = isValidMove(board, tile, xstart, ystart)
  if tilesToFlip == False:
      return False
  board[xstart][ystart] = tile
  for x, y in tilesToFlip:
      board[x][y] = tile
  return True


def isOnCorner(x, y):
     # Returns True if the position is in one of the four corners.
  return (x == 0 and y == 0) or (x == 7 and y == 0) or (x == 0 and y == 7) or (x == 7 and y == 7)

#REWORK
def getPlayerMove(board, playerTile,x1,y1):
     # Let the player type in their move.
     # Returns the move as [x, y] (or returns the strings 'hints' or 'quit')
  DIGITS1TO8 = [0,1,2,3,4,5,6,7]
  while True:
      row,col = getRowCol(x1,y1)
      
      if row in DIGITS1TO8 and col in DIGITS1TO8:
          
          if isValidMove(board, playerTile, row, col) == False:
              return False
          else:
              break
      else:
          print('That is not a valid move. Type the x digit (1-8), then the y digit (1-8).')
          print('For example, 81 will be the top-right corner.')
          return False
  return [row, col]

def showPoints(playerTile, computerTile, mainBoard):
     # Prints out the current score.
  scores = getScoreOfBoard(mainBoard)
  print('You have %s points. The computer has %s points.' % (scores[playerTile], scores[computerTile]))
  
def getComputerMove(board, computerTile):
     # Given a board and the computer's tile, determine where to
     # move and return that move as a [x, y] list.
  possibleMoves = getValidMoves(board, computerTile)
     # randomize the order of the possible moves
  random.shuffle(possibleMoves)
     # always go for a corner if available.
  for x, y in possibleMoves:
      if isOnCorner(x, y):
          return [x, y]
     # Go through all the possible moves and remember the best scoring move
  bestScore = -1
  for x, y in possibleMoves:
      dupeBoard = getBoardCopy(board)
      makeMove(dupeBoard, computerTile, x, y)
      score = getScoreOfBoard(dupeBoard)[computerTile]
      if score > bestScore:
          bestMove = [x, y]
          bestScore = score
  return bestMove



def main():
    run = True
    clock = pygame.time.Clock()
    # Reset the board and game.
    mainBoard = getNewBoard()
    resetBoard(mainBoard)
    playerTile, computerTile = 'X','O'
    turn = 'X'
    #rect1 = pygame.Rect(0,0,20,20)
    #rect2 = pygame.Rect(20,20,30,30)
    #rect3 = pygame.Rect(5,5,5,5)
    #pygame.draw.rect(WIN,(255,0,0),rect1,0)
    #pygame.draw.rect(WIN,(0,0,255),rect2,0)
    #pygame.draw.rect(WIN,(0,255,255),rect3,0)
    finish = False
    drawBoard(mainBoard)
    
    
    
    while run:
        ##Othello logic
        print(turn)
        if(finish):
            break
        
        clock.tick(FPS)
        
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                run = False
                
            if event.type == pygame.MOUSEBUTTONDOWN and turn == 'X':
                print("x")
                x,y = pygame.mouse.get_pos()
                move = getPlayerMove(mainBoard, playerTile,x,y)
                if(move!=False):
                    makeMove(mainBoard, playerTile, move[0], move[1])
                    turn = 'O'
                
            if getValidMoves(mainBoard, computerTile) == []:
                turn = 'X'
                if getValidMoves(mainBoard, playerTile) == []:
                    finish=True
                    continue
            
                
            drawBoard(mainBoard)
            
        if turn == 'O':
            
            #showPoints(playerTile, computerTile)
            x, y = getComputerMove(mainBoard, computerTile)
            makeMove(mainBoard, computerTile, x, y)
            if getValidMoves(mainBoard, playerTile) == []:
                
                turn = 'O'
                if getValidMoves(mainBoard, computerTile) == []:
                    finish=True
                    continue
            else:
                turn = 'X'
            
            drawBoard(mainBoard)
                
        
        pygame.display.update()
    pygame.quit()
    
    
main()