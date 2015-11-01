datatype size =
         DefaultSize
       | Pixels of int
       | Percent of int

datatype button =
         Separator

       | Source
       | Save
       | NewPage
       | DocProps
       | Preview
       | Print
       | Templates
       | Document

       | Cut
       | Copy
       | Paste
       | PasteText
       | PasteFromWord
       | Undo
       | Redo

       | Find
       | Replace
       | SelectAll
       | Scayt

       | Form
       | Checkbox
       | Radio
       | TextField
       | Textarea
       | Select
       | Button
       | ImageButton
       | HiddenField

       | Bold
       | Italic
       | Underline
       | Strike
       | Subscript
       | Superscript
       | RemoveFormat

       | NumberedList
       | BulletedList
       | Outdent
       | Indent
       | Blockquote
       | CreateDiv
       | JustifyLeft
       | JustifyCenter
       | JustifyRight
       | JustifyBlock
       | BidiLtr
       | BidiRtl

       | Link
       | Unlink
       | Anchor

       | CreatePlaceholder
       | Image
       | Flash
       | Table
       | HorizontalRule
       | Smiley
       | SpecialChar
       | PageBreak
       | Iframe
       | InsertPre

       | Styles
       | Format
       | Font
       | FontSize

       | TextColor
       | BGColor

       | UIColor
       | Maximize
       | ShowBlocks
  	
       | Button1
       | Button2
       | Button3
       | Oembed
       | MediaEmbed

       | About

datatype toolbar =
         Newline
       | Bar of { Nam : option string, Buttons : list button }

datatype toolbar_set =
         DefaultToolbarSet
       | Custom of list toolbar

type editor

val editor : {Width : size,
              Height : size,
              ToolbarSet : toolbar_set,
              InitialText : string}
             -> transaction editor
val show : editor -> xbody
val content : editor -> signal string
val setContent : editor -> string -> transaction unit
