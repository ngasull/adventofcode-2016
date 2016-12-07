const fs = require('fs')
const path = require('path')
const R = require('ramda')

const part1 = R.pipe(
  R.map(([lettersDashes, sectorId, checkSum]) =>
    computeCheckSum(lettersDashes) === checkSum ? sectorId : 0
  ),
  R.sum)

const part2 = R.pipe(
  R.map(([lettersDashes, sectorId]) =>
    [unshiftCifer(sectorId)(lettersDashes), sectorId]
  ),
  R.filter(R.test(/north/)))

const computeCheckSum = R.pipe(
  R.replace(/-/g, ''),
  R.split(''),
  R.countBy(R.identity),
  obj => R.keys(obj).map(l => [l, obj[l]]),
  R.sort(([l1, c1], [l2, c2]) => c2 - c1 || l1.charCodeAt(0) - l2.charCodeAt(0)),
  R.map(R.prop(0)),
  R.take(5),
  R.join(''))

const charInitialIndex = 'a'.charCodeAt(0)

const unshiftCifer = sectorId => R.pipe(
  R.split(''),
  R.map(l =>
    l === '-' ?
      ' '
    :
      String.fromCharCode(
        ((l.charCodeAt(0) - charInitialIndex + sectorId) % 26) + charInitialIndex
      )
  ),
  R.join(''))

{
  const rooms = fs.readFileSync(path.join(__dirname, 'input.txt'))
    .toString()
    .split('\n')
    .map(r => {
      const match = r.match(/^([a-z-]+)-(\d+)\[([a-z]{5})\]$/)
      return match ? [match[1], parseInt(match[2], 10), match[3]] : null
    })
    .filter(r => !!r)

  console.log(part1(rooms))
  console.log(part2(rooms))
}
